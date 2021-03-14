-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

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
import Data.Default
import Data.Foldable
import Data.Function (on)
import Data.Graph hiding (Vertex, Edge, vertices, edges, components)
import qualified Data.Graph as Graph
import Data.HashMap.Lazy (unionWith)
import qualified Data.HashMap.Lazy as HashMap
import Data.IntMap (insertWith, keys, adjust, member, deleteMin, deleteMax)
import qualified Data.IntMap as IntMap
import Data.List (sortOn, groupBy, partition)
import Data.Maybe
import qualified Data.Sequence as Seq
import Data.STRef
import Data.Tuple
import Data.Vector (Vector, (!), fromListN, unsafeThaw, unsafeIndex, accum, accumulate, generate, replicate, unsafeUpd)
import qualified Data.Vector as Vector
import Data.Vector.Mutable (modify)

import Prelude hiding (replicate)

import LSC.Component
import LSC.NetGraph
import LSC.SegmentTree
import LSC.Polygon
import LSC.Types
import LSC.UnionFind



wt1, wt2 :: Int
wt1 = 1
wt2 = 1


type EdgeDisjoint s = (Vertex (DisjointSet s), Vertex (DisjointSet s))

type Edge = (Vertex Int, Vertex Int)


isBuiltIn :: (Vertex a, Vertex a) -> Bool
isBuiltIn (u, v) = cell u `eqNumber` cell v


data Vertex a = Vertex
  { rowIndex  :: Int
  , colIndex  :: Int
  , net       :: Net
  , cell      :: Gate
  , point     :: a
  }


channelIndex :: Vertex Int -> Int
channelIndex u | even $ point u = pred $ rowIndex u
channelIndex u = succ $ rowIndex u


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

    let row :: Gate -> Maybe Row
        row g = top ^. supercell . rows ^? ix (g ^. space . b)

    assume "globalDetermineFeedthroughs: gates are not aligned to rows!"
      $ all (isJust . row) $ top ^. gates


    let sameRow ys = [ (x, y) | xs <- ys, ((x, _), (y, _)) <- zip xs (tail xs) ]

    let diffRow zs = [ (x, y) | (xs, ys) <- distinctPairs zs, (_, x) <- xs, (y, _) <- ys ]

    builtInEdges <- forM (view nets top) $ \ n ->
        forM (verticesByRow top n) $ mapM $ \ (g, p) ->
            edgeDisjoint g n
            (maybe 0 (view number) (row g))
            (centerX $ coarseBoundingBox $ p ^. geometry <&> containingBox)
            -- ^ the built-in edges for all pins in the layout grouped by net and row


    vertices <- newSTRef $ foldMap Vector.fromList <$> builtInEdges

    edges <- newSTRef $ foldMap (Seq.fromList . liftA2 (++) sameRow diffRow) builtInEdges

    let weight ps (u, v)
            = wt1 * abs (colIndex u - colIndex v)
            + wt2 * sum [ unsafeIndex ps i | i <- [ rowIndex u + 1 .. rowIndex v - 1] ]
            -- rowIndex u <= rowIndex v

    penalties <- unsafeThaw
      $ accumulate (+) (0 <$ ss)
      $ Vector.zip
        (maybe 0 (view number) . row <$> view gates top)
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

        cyclic <- point u `equivalent` point v

        unless cyclic
          $ if rowIndex v - rowIndex u <= 1 -- rowIndex u <= rowIndex v
            then point u `union` point v
            else do

                intersections <- sequence $ generate (rowIndex v - rowIndex u - 1) $ \ i ->
                    edgeDisjoint (feedthroughGate (net u)) (net u)
                    (rowIndex u + succ i)
                    (colIndex u + div (succ i * (colIndex v - colIndex u)) (rowIndex v - rowIndex u))
                    -- ^ the built-in edges for feedthroughs

                sequence_
                  $ Vector.zipWith union
                    (point . snd <$> intersections)
                    (point . fst <$> Vector.tail intersections)
                    -- ^ the chain of feedthroughs is interconnected

                -- connect feedthroughs to every pin in the net
                subgraph <- view (net u ^. identifier . to ix) <$> readSTRef vertices
                modifySTRef edges $ (<>) $ Seq.fromList
                  [ if rowIndex upperFeed > rowIndex upperComp
                    then (lowerComp, upperFeed)
                    else (lowerFeed, upperComp)
                  | (upperFeed, lowerFeed) <- toList intersections
                  , (upperComp, lowerComp) <- toList subgraph
                  ]

                -- adjust penalties for rows containing feedthrough
                for_ intersections $ \ (i, _) -> modify penalties (+ ss ! rowIndex i ^. granularity) (rowIndex i)

                -- add feedthroughs to its net
                modifySTRef vertices $ HashMap.insertWith (<>) (net u ^. identifier) intersections

                -- save feedthroughs to the netgraph
                modifySTRef feedthroughs $ HashMap.insertWith (<>) (net u ^. identifier) intersections

    intersections <- readSTRef feedthroughs

    let gate :: Vertex a -> Gate
        gate u = cell u & space %~ relocateL (colIndex u) . relocateB (ss ! rowIndex u ^. b)

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

    let row :: Gate -> Maybe Row
        row g = top ^. supercell . rows ^? ix (g ^. space . b)

    assume "globalDetermineNetSegments: gates are not aligned to rows!"
      $ all (isJust . row) $ top ^. gates


    let builtInEdges = flip fmap (view nets top) $ \ n ->
            flip fmap (zip [0..] $ fold $ verticesByColumn top n) $ \ (k, (g, p)) ->
                edgeCounter k g n
                (maybe 0 (view number) (row g))
                (centerX $ coarseBoundingBox $ containingBox <$> p ^. geometry)
                -- ^ the built-in edges for all pins in the layout grouped by net
                --

    let simplifiedNetConnectionGraph = simplifiedNetConnection <$> builtInEdges


    let vertices = foldMap (fromListN 2 . toListOf both) <$> builtInEdges

    edges <- newSTRef $ buildGraph <$> unionWith (++) builtInEdges simplifiedNetConnectionGraph

    let weight d (u, v) = densityRatio (colIndex u, colIndex v) (d ! channelIndex u)


    cycles <- newSTRef . imap (edgesInSomeCycle . (vertices ^.) . ix) =<< readSTRef edges

    densities <- unsafeThaw
      . fmap constructSegmentTree
      . accum (flip (:)) (replicate (length rs + 1) [])
      . map (liftA2 (,) (channelIndex . fst) (bimap colIndex colIndex))
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

        modify densities (pull (colIndex u, colIndex v)) (channelIndex u) 

    components <- readSTRef edges

    let locate u | even $ point u = (colIndex u, ss ! rowIndex u ^. b)
        locate u = (colIndex u, ss ! succ (rowIndex u) ^. b)

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
  = foldMap (liftA2 zip id tail . sortOn colIndex)
  . groupBy ((==) `on` channelIndex)
  . sortOn channelIndex
  . foldMap (toListOf both)



buildGraph :: [Edge] -> Graph
buildGraph
  = liftA2 buildG ((,) <$> minimum . map fst <*> maximum . map snd) ((++) <$> id <*> map swap)
  . fmap (bimap point point)



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
  [ (point u, filter (/= point v) $ g ^. ix (point u))
  , (point v, filter (/= point u) $ g ^. ix (point v))
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

    swaps zs (e : es) (x : xs) = swaps (e : x : zs) es xs
    swaps zs es xs = fst fs ++ reverse zs ++ xs ++ snd fs where fs = splitAt (length es `div` 2) es

    relocated
      = zipWith (map . over space . relocateB) (top ^. supercell . rows . to keys)
      . uncurry (swaps [])
      . partition null
      . toList
      . fmap (filter (not . view fixed))
      $ foldl' (\ a g -> insertWith (++) (g ^. space . b) [g] a)
        (top ^. supercell . rows <&> const [])
        (top ^. gates)



densityRowSpacing :: NetGraph -> ST s NetGraph
densityRowSpacing top = do
    pure top



debugChannelDensities :: NetGraph -> LSC ()
debugChannelDensities top = do

    let j = length . show $ length densities
        k = length . show $ maximum densities

    let padLeft n s = take (n - length s) (repeat ' ') ++ s

    debug
      $ "Channel densities:" :
      [ "Channel " ++ padLeft j (show i) ++ ": " ++ padLeft k (show d)
      | (i, d) <- zip [ 1 :: Int .. ] densities
      ]

  where

    densities
      = map (maxDensity . constructSegmentTree)
      . uncurry (zipWith (++))
      . bimap toList toList
      . foldl' buckets (deleteMin (channels b), deleteMax (channels t))
      . foldMap (view netSegments)
      $ top ^. nets

    buckets (tops, bottoms) (Line (x1, y1) (x2, y2))
      | x1 == x2 -- built-in edge
      , min y1 y2 `member` tops
      = (tops, bottoms)
    buckets (tops, bottoms) (Line (x1, y1) (x2, y2))
      | y1 == y2 -- same row
      = (adjust ((x1, x2) :) y1 tops, adjust ((x1, x2) :) y2 bottoms)
    buckets (tops, bottoms) (Line (x1, y1) (x2, y2))
      = (tops, adjust ((x1, x2) :) (min y1 y2) bottoms)

    channels f
      = fmap (const [])
      $ IntMap.filter id
      $ foldl'
        (\ a g -> adjust (\ !h -> h || not (g ^. feedthrough) && not (g ^. fixed)) (g ^. space . f) a)
        (False <$ top ^. supercell . rows)
        (top ^. gates)

