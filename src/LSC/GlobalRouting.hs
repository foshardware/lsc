-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}

module LSC.GlobalRouting
  ( determineFeedthroughs, globalDetermineFeedthroughs
  , determineNetSegments, globalDetermineNetSegments
  , determineRowSpacing, densityRowSpacing
  ) where

#if !MIN_VERSION_base(4,10,0)
import Data.Semigroup ((<>))
#endif

import Control.Applicative
import Control.Lens hiding (ifoldl')
import Control.Monad
import Control.Monad.Loops
import Control.Monad.ST
import Data.Array ((//))
import Data.Bifoldable
import Data.Copointed
import Data.Default
import Data.Foldable
import Data.Graph hiding (Vertex, Edge, vertices, edges, components)
import qualified Data.Graph as Graph
import Data.HashMap.Lazy (unionWith)
import qualified Data.HashMap.Lazy as HashMap
import Data.IntMap (keys, adjust, deleteMin, deleteMax)
import qualified Data.IntMap as IntMap
import Data.List (sortOn, intersperse)
import Data.Maybe
import qualified Data.Sequence as Seq
import Data.STRef
import Data.Tuple
import Data.Vector
  ( Vector, (!)
  , fromListN
  , unsafeThaw, unsafeIndex, unsafeUpd
  , accum, accumulate
  , generate, replicate
  )
import qualified Data.Vector as Vector
import Data.Vector.Mutable (modify)

import Prelude hiding (replicate)

import LSC.Cartesian
import LSC.Component
import LSC.NetGraph
import LSC.SegmentTree (pull, maxDensity, densityRatio)
import qualified LSC.SegmentTree as SegmentTree
import LSC.Polygon
import LSC.Types
import LSC.UnionFind



wt1, wt2 :: Int
wt1 = 1
wt2 = 1


type EdgeDisjoint s = (Vertex (DisjointSet s), Vertex (DisjointSet s))

type Edge = (Vertex Int, Vertex Int)


isBuiltIn :: (Vertex a, Vertex a) -> Bool
isBuiltIn (u, v) = cell u ^. number == cell v ^. number


data Vertex a = Vertex
  { ordinate  :: Int
  , abscissa  :: Int
  , net       :: Net
  , cell      :: Gate
  , payload   :: a
  } deriving Functor

instance Copointed Vertex where copoint = payload


channel :: Vertex Int -> Int
channel u | even $ copoint u = pred $ ordinate u
channel u = succ $ ordinate u


edgeCounter :: Int -> Gate -> Net -> Int -> Int -> Edge
edgeCounter k g n i j = (Vertex i j n g x, Vertex i j n g $ succ x)
  where x = k * 2


edgeDisjoint :: Gate -> Net -> Int -> Int -> ST s (EdgeDisjoint s)
edgeDisjoint g n i j = do
    (x, y) <- pair
    pure (Vertex i j n g x, Vertex i j n g y)




determineFeedthroughs :: NetGraph -> LSC NetGraph
determineFeedthroughs top = do
    info ["Determine feedthroughs"]
    realWorldST . globalDetermineFeedthroughs $ top


-- | O(n^2) in the number of pins
--
globalDetermineFeedthroughs :: NetGraph -> ST s NetGraph
globalDetermineFeedthroughs top = do

    let rs = top ^. supercell . rows
        ss = fromListN (length rs) (toList rs)

    assume "globalDetermineFeedthroughs: gates are not aligned to rows!"
      $ all (isJust . getRow top) $ top ^. gates


    let sameRow ys = [ (x, y) | xs <- ys, ((x, _), (y, _)) <- zip xs (tail xs) ]

    let diffRow zs = [ (x, y) | (xs, ys) <- distinctPairs zs, (_, x) <- xs, (y, _) <- ys ]

    builtInEdges <- forM (view nets top) $ \ n ->
        forM (verticesByRow top n) $ mapM $ \ (g, p) ->
            edgeDisjoint g n
            (maybe 0 (view number) (getRow top g))
            (centerX $ coarseBoundingBox $ p ^. geometry <&> containingBox)
            -- ^ the built-in edges for all pins in the layout grouped by net and row


    vertices <- newSTRef $ foldMap Vector.fromList <$> builtInEdges

    edges <- newSTRef $ foldMap (Seq.fromList . liftA2 (++) sameRow diffRow) builtInEdges

    let weight ps (u, v)
            = wt1 * abs (abscissa u - abscissa v)
            + wt2 * sum [ unsafeIndex ps i | i <- [ ordinate u + 1 .. ordinate v - 1] ]
            -- ordinate u <= ordinate v

    penalties <- unsafeThaw
      $ accumulate (+) (0 <$ ss)
      $ Vector.zip
        (maybe 0 (view number) . getRow top <$> view gates top)
        (gateWidth <$> view gates top)

    feedthroughs <- newSTRef mempty

    whileM_ (not . null <$> readSTRef edges)
      $ do

        ps <- Vector.freeze penalties

        (_, (pos, (u, v))) <- ifoldl'
            (\ i (w, e) f -> let x = weight ps f in if x < w then (x, (i, f)) else (w, e))
            (maxBound, undefined) <$> readSTRef edges
            -- ^ find the minimum weighted edge and its position in the sequence

        modifySTRef edges $ Seq.deleteAt pos

        cyclic <- copoint u `equivalent` copoint v

        unless cyclic
          $ if ordinate v - ordinate u <= 1 -- ordinate u <= ordinate v
            then copoint u `union` copoint v
            else do

                intersections <- sequence $ generate (ordinate v - ordinate u - 1) $ \ i ->
                    edgeDisjoint (feedthroughGate (net u)) (net u)
                    (ordinate u + succ i)
                    (abscissa u + div (succ i * (abscissa v - abscissa u)) (ordinate v - ordinate u))
                    -- ^ the built-in edges for feedthroughs

                sequence_
                  $ Vector.zipWith union
                    (copoint . snd <$> intersections)
                    (copoint . fst <$> Vector.tail intersections)
                    -- ^ the chain of feedthroughs is interconnected

                -- connect feedthroughs to every pin in the net
                subgraph <- view (net u ^. identifier . to ix) <$> readSTRef vertices
                modifySTRef edges $ (<>) $ Seq.fromList
                  [ if ordinate upperFeed > ordinate upperComp
                    then (lowerComp, upperFeed)
                    else (lowerFeed, upperComp)
                  | (upperFeed, lowerFeed) <- toList intersections
                  , (upperComp, lowerComp) <- toList subgraph
                  ]

                -- adjust penalties for rows containing feedthrough
                for_ intersections
                  $ \ (i, _) -> modify penalties (+ ss ! ordinate i ^. granularity) (ordinate i)

                -- add feedthroughs to its net
                modifySTRef vertices $ HashMap.insertWith (<>) (net u ^. identifier) intersections

                -- save feedthroughs to the netgraph
                modifySTRef feedthroughs $ HashMap.insertWith (<>) (net u ^. identifier) intersections

    intersections <- readSTRef feedthroughs

    let gate :: Vertex a -> Gate
        gate u = cell u & space %~ relocateL (abscissa u) . relocateB (ss ! ordinate u ^. b)

    pure $ top &~ do
        gates <>= foldMap (gate . fst <$>) intersections
        gates  %= imap (set number)



feedthroughGate :: Net -> Gate
feedthroughGate n = def &~ do
    identifier .= ("FEEDTHROUGH." <> n ^. identifier)
    feedthrough .= True
    wires .= HashMap.singleton "FD" (n ^. identifier)





determineNetSegments :: NetGraph -> LSC NetGraph
determineNetSegments top = do
    info ["Determine net segments"]
    result <- realWorldST . globalDetermineNetSegments $ top
    debugChannelDensities result
    pure result



globalDetermineNetSegments :: NetGraph -> ST s NetGraph
globalDetermineNetSegments top = do

    let rs = top ^. supercell . rows
        ss = fromListN (length rs) (toList rs)

    assume "globalDetermineNetSegments: gates are not aligned to rows!"
      $ all (isJust . getRow top) $ top ^. gates


    let builtInEdges = view nets top
          <&> \ n -> [0 ..] `zip` fold (verticesByColumn top n)
          <&> \ (k, (g, p)) -> edgeCounter k g n
                (maybe 0 (view number) (getRow top g))
                (centerX $ coarseBoundingBox $ p ^. geometry <&> containingBox)
                -- the built-in edges for all pins in the layout grouped by net
                --

    let simplifiedNetConnectionGraph = simplifiedNetConnection <$> builtInEdges


    let vertices = foldMap (fromListN 2 . biList) <$> builtInEdges

    edges <- newSTRef $ buildGraph <$> unionWith (++) builtInEdges simplifiedNetConnectionGraph

    let weight d (u, v) = densityRatio (abscissa u, abscissa v) (d ! channel u)


    cycles <- newSTRef . imap (edgesInSomeCycle . (vertices ^.) . ix) =<< readSTRef edges

    densities <- unsafeThaw
      . fmap SegmentTree.fromList
      . accum (flip (:)) (replicate (length rs + 1) [])
      . map (liftA2 (,) (channel . fst) (bimap abscissa abscissa))
      . filter (not . isBuiltIn)
      . fold
      $ simplifiedNetConnectionGraph


    whileM_ (not . all null <$> readSTRef cycles)
      $ do

        d <- Vector.freeze densities

        (_, (u, v)) <- (foldl' . foldl')
            (\ (w, e) f -> let x = weight d f in if x > w then (x, f) else (w, e))
            (-1, undefined) <$> readSTRef cycles
            -- ^ find the maximum weighted edge

        let n = u ^. to net . identifier

        Just unburdened <- fmap (delete (u, v)) . preview (ix n) <$> readSTRef edges

        modifySTRef edges  $ HashMap.insert n $ unburdened
        modifySTRef cycles $ HashMap.insert n $ edgesInSomeCycle (vertices ^. ix n) unburdened

        modify densities (pull (abscissa u, abscissa v)) (channel u)

    components <- readSTRef edges

    let locate u | even $ copoint u = (abscissa u, ss ! ordinate u ^. b)
        locate u = (abscissa u, ss ! succ (ordinate u) ^. b)

    let draw :: Identifier -> Net -> Net
        draw i
          = (<>~) netSegments
          . map (view line . bimap locate locate)
          . filter (not . isBuiltIn)
          . foldMap (upward $ vertices ^. ix i)
          $ components ^? ix i

    pure $ top &~ do
        nets %= imap draw




simplifiedNetConnection :: [Edge] -> [Edge]
simplifiedNetConnection
  = foldMap (liftA2 zip id tail . sortOn abscissa)
  . groupOn channel
  . sortOn channel
  . foldMap biList 



buildGraph :: [Edge] -> Graph
buildGraph
  = liftA2 buildG ((,) <$> minimum . map fst <*> maximum . map snd) ((++) <$> id <*> map swap)
  . fmap (bimap copoint copoint)



edgesInSomeCycle :: Vector (Vertex Int) -> Graph -> [Edge]
edgesInSomeCycle xs g =
  [ (xs ! u, xs ! v)
  | s <- cyclic =<< bcc g
  , u <- s
  , v <- g ^. ix u
  , u < v
  , not $ isBuiltIn (xs ! u, xs ! v)
  , elem v s
  ] where cyclic (Node s@(_ : _ : _ : _) ss) = s : (cyclic =<< ss)
          cyclic (Node _                 ss) =      cyclic =<< ss



upward :: Vector (Vertex Int) -> Graph -> [Edge]
upward xs g = [ (xs ! u, xs ! v) | (u, v) <- Graph.edges g, u < v ]



delete :: Edge -> Graph -> Graph
delete (u, v) g = g //
  [ (copoint u, filter (/= copoint v) $ g ^. ix (copoint u))
  , (copoint v, filter (/= copoint u) $ g ^. ix (copoint v))
  ]




determineRowSpacing :: NetGraph -> LSC NetGraph
determineRowSpacing top
  | all (views netSegments null) (view nets top)
  = do
    info ["Determine row spacing"]
    pure . oneRowSpacing $ top
determineRowSpacing top
  = do
    info ["Determine row spacing"]
    realWorldST . densityRowSpacing $ top



oneRowSpacing :: NetGraph -> NetGraph
oneRowSpacing top = top &~ do

    gates %= flip unsafeUpd (liftA2 (,) (view number) id <$> fold relocated)

  where

    riffle xs = take k (repeat []) ++ intersperse [] xs ++ repeat []
      where k = div (top ^. supercell . rows . to length - 2 * length xs) 2

    relocated
      = zipWith (map . over space . relocateB) (top ^. supercell . rows . to keys)
      . riffle
      . filter (not . null)
      . toList
      . foldl' (\ a g -> adjust (g :) (g ^. space . b) a) ([] <$ top ^. supercell . rows)
      . Vector.filter (not . view fixed)
      $ top ^. gates



densityRowSpacing :: NetGraph -> ST s NetGraph
densityRowSpacing top = do
    pure top



debugChannelDensities :: NetGraph -> LSC ()
debugChannelDensities top = do

    debug
      $ "Channel densities:" :
      [ "Channel " ++ show i ++ ": " ++ show d
      | (i, d) <- zip [ 1 :: Int .. ] densities
      ]

  where

    densities
      = map (maxDensity . SegmentTree.fromList)
      . uncurry (zipWith (++))
      . bimap toList toList
      . foldl' buckets (deleteMin (channels b), deleteMax (channels t))
      . foldMap (view netSegments)
      $ top ^. nets

    buckets (tops, bottoms) (Line (x1, y1) (x2, y2))
      | y1 == y2
      = (adjust ((x1, x2) :) y1 tops, adjust ((x1, x2) :) y2 bottoms)
    buckets (tops, bottoms) (Line (x1, y1) (x2, y2))
      = (tops, adjust ((x1, x2) :) (min y1 y2) bottoms)

    channels f
      = fmap (const [])
      $ IntMap.filter id
      $ foldl'
        (\ a g -> adjust (\ !h -> h || not (g ^. fixed)) (g ^. space . f) a)
        (False <$ top ^. supercell . rows)
        (top ^. gates)

