-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module LSC.GlobalRouting
    ( determineFeedthroughs
    , globalDetermineFeedthroughs
    , determineNetSegments
    , globalDetermineNetSegments
    ) where

#if MIN_VERSION_base(4,10,0)
#else
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
import Data.Graph hiding (Vertex, vertices, edges, components)
import qualified Data.Graph as Graph
import qualified Data.HashMap.Lazy as HashMap
import Data.List (sortOn, groupBy)
import Data.Maybe
import qualified Data.Sequence as Seq
import Data.STRef
import Data.Vector ((!), fromListN, unsafeThaw, unsafeIndex, accumulate, generate)
import qualified Data.Vector as Vector
import Data.Vector.Mutable (modify)

import LSC.Component
import LSC.NetGraph
import LSC.SegmentTree
import LSC.Types
import LSC.UnionFind



wt1, wt2 :: Int
wt1 = 1
wt2 = 1


type DisjointEdge s = (Vertex (DisjointSet s), Vertex (DisjointSet s))

type EnumEdge = (Vertex Int, Vertex Int)


data Vertex a = Vertex
  { rowIndex  :: Int
  , colIndex  :: Int
  , net       :: Identifier
  , point     :: a
  } deriving Eq


channelIndex :: Vertex Int -> Int
channelIndex u = rowIndex u + point u `mod` 2


enumEdge :: STRef s Int -> Identifier -> Int -> Int -> ST s EnumEdge
enumEdge counter n i j = do
    x <- readSTRef counter
    modifySTRef counter (+ 2)
    pure (Vertex i j n x, Vertex i j n $ succ x)


disjointEdge :: Identifier -> Int -> Int -> ST s (DisjointEdge s)
disjointEdge n i j = do
    (x, y) <- pair
    pure (Vertex i j n x, Vertex i j n y)




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
            disjointEdge (view identifier n)
            (maybe 0 (view number) (row g))
            (centerX $ head $ view geometry p)
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
                    disjointEdge (net u)
                    (rowIndex u + succ i)
                    (colIndex u + div (succ i * (colIndex v - colIndex u)) (rowIndex v - rowIndex u))
                    -- ^ the built-in edges for feedthroughs

                sequence_
                  $ Vector.zipWith union
                    (point . snd <$> intersections)
                    (point . fst <$> Vector.tail intersections)
                    -- ^ the chain of feedthroughs is interconnected

                -- connect feedthroughs to every pin in the net
                subgraph <- view (ix (net u)) <$> readSTRef vertices
                modifySTRef edges $ mappend $ Seq.fromList
                  [ if rowIndex upperFeed > rowIndex upperComp
                    then (lowerComp, upperFeed)
                    else (lowerFeed, upperComp)
                  | (upperFeed, lowerFeed) <- toList intersections
                  , (upperComp, lowerComp) <- toList subgraph
                  ]

                -- adjust penalties for rows containing feedthrough
                for_ intersections $ \ (i, _) -> modify penalties (+ ss ! rowIndex i ^. granularity) (rowIndex i)

                -- add feedthroughs to its net
                modifySTRef vertices $ HashMap.insertWith (<>) (net u) intersections

                -- save feedthroughs to the netgraph
                modifySTRef feedthroughs $ HashMap.insertWith (<>) (net u) intersections

    intersections <- readSTRef feedthroughs

    let locate :: Vertex a -> Gate -> Gate
        locate u = space %~ relocateL (colIndex u) . relocateB (ss ! rowIndex u ^. b)

    pure $ top &~ do
        gates <>= ifoldMap (fmap . flip (locate . fst) . feedthroughGate) intersections
        gates  %= imap (set number)



feedthroughGate :: Identifier -> Gate
feedthroughGate n = def &~ do
    identifier .= ("FEEDTHROUGH." <> n)
    feedthrough .= True
    wires .= HashMap.singleton "FD" n





determineNetSegments :: NetGraph -> LSC NetGraph
determineNetSegments top = do
    info ["Determine net segments"]
    realWorldST . globalDetermineNetSegments $ top



globalDetermineNetSegments :: NetGraph -> ST s NetGraph
globalDetermineNetSegments top = do

    let rs = top ^. supercell . rows
        ss = fromListN (length rs) (toList rs)

    let row :: Gate -> Maybe Row
        row g = top ^. supercell . rows ^? ix (g ^. space . b)

    assume "globalDetermineNetSegments: gates are not aligned to rows!"
      $ all (isJust . row) $ top ^. gates


    builtInEdges <- forM (view nets top) $ \ n -> do
        counter <- newSTRef 0
        forM (verticesOf top n) $ \ (g, p) ->
            enumEdge counter (view identifier n)
            (maybe 0 (view number) (row g))
            (centerX $ head $ view geometry p)


    let vertices = foldMap (\ (u, v) -> Vector.fromList [u, v]) <$> builtInEdges

    edges <- newSTRef $ buildGraph . liftA2 (++) id simplifiedNetConnection <$> builtInEdges

    cycles <- newSTRef . fmap edgesInSomeCycle =<< readSTRef edges

    let weight xs (u, v) = rowIndex (xs ! v) - rowIndex (xs ! u) + abs (colIndex (xs ! u) - colIndex (xs ! v))


    whileM_ (not . all null <$> readSTRef cycles)
      $ do

        (xs, (u, v)) <- maximumBy (compare `on` uncurry weight) . ifoldMap (\ i g -> (vertices ^. ix i, ) <$> g)
            <$> readSTRef cycles

        let n = xs ^. ix u . to net

        Just unburdened <- fmap (delete (u, v)) . preview (ix n) <$> readSTRef edges

        modifySTRef edges  $ HashMap.insert n $ unburdened
        modifySTRef cycles $ HashMap.insert n $ edgesInSomeCycle unburdened


    components <- readSTRef edges

    let locate u | even $ point u = (colIndex u, ss ! rowIndex u ^. b)
        locate u = (colIndex u, ss ! succ (rowIndex u) ^. b)

    let vertex i x = locate $ view (ix i) vertices ! x 

    let draw :: Identifier -> Net -> Net
        draw i = netSegments .~ fmap (view line . bimap (vertex i) (vertex i)) (views (ix i) upwards components)

    pure $ top &~ do
        nets %= imap draw




simplifiedNetConnection :: [EnumEdge] -> [EnumEdge]
simplifiedNetConnection
  = foldMap (liftA2 zip id tail . sortOn colIndex)
  . groupBy ((==) `on` channelIndex)
  . sortOn channelIndex
  . foldMap (\ (u, v) -> [u, v])



buildGraph :: [EnumEdge] -> Graph
buildGraph xs
  = buildG (0, maximum $ point . snd <$> xs)
  $ liftA2 (++) id (map swap)
  $ bimap point point <$> xs
    where swap ~(x, y) = (y, x)



edgesInSomeCycle :: Graph -> [Edge]
edgesInSomeCycle g =
  [ (u, v)
  | s <- cyclic =<< bcc g
  , u <- s
  , v <- g ^. ix u
  , u < v
  , elem v s
  ] where cyclic (Node s@(_ : _ : _ : _) ss) = s : (cyclic =<< ss)
          cyclic (Node _                 ss) =      cyclic =<< ss



delete :: Edge -> Graph -> Graph
delete (u, v) g = g // [ (u, filter (/= v) (g ^. ix u)), (v, filter (/= u) (g ^. ix v)) ]


upwards :: Graph -> [Edge]
upwards g = [ (u, v) | (u, v) <- Graph.edges g, u < v ]

