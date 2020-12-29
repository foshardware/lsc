-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE OverloadedStrings #-}

module LSC.GlobalRouting
    ( determineFeedthroughs
    , globalDetermineFeedthroughs
    ) where

import Control.Applicative
import Control.Lens hiding (ifoldl')
import Control.Monad
import Control.Monad.Loops
import Control.Monad.ST
import Data.Default
import Data.Foldable
import qualified Data.HashMap.Lazy as HashMap
import Data.Maybe
import qualified Data.Sequence as Seq
import Data.STRef
import Data.Vector ((!), fromListN, unsafeThaw, unsafeIndex, accumulate, generate)
import qualified Data.Vector as Vector
import Data.Vector.Mutable (modify)

import LSC.Component
import LSC.NetGraph
import LSC.Types
import LSC.UnionFind



wt :: Int
wt = 1


type Edge s = (Vertex s, Vertex s)


data Vertex s = Vertex
  { rowIndex  :: Int
  , colIndex  :: Int
  , net       :: Identifier
  , point     :: DisjointSet s
  }


freeEdge :: Identifier -> Int -> Int -> ST s (Edge s)
freeEdge n i j = do
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


    let builtInEdge :: Net -> Gate -> Pin -> ST s (Edge s)
        builtInEdge n g p
            = freeEdge (view identifier n)
              (maybe 0 (view number) (row g))
              (centerX $ coarseBoundingBox $ view geometry p)

    let sameRow ys = [ (x, y) | xs <- ys, ((x, _), (y, _)) <- zip xs (tail xs) ]

    let diffRow zs = [ (x, y) | (xs, ys) <- distinctPairs zs, (_, x) <- xs, (y, _) <- ys ]

    builtInEdges <- views nets forM top $ \ n ->
        forM (verticesByRow top n) $ mapM $ uncurry $ builtInEdge n
        -- ^ the built-in edges for all pins in the layout grouped by net and row


    vertices <- newSTRef $ foldMap Vector.fromList <$> builtInEdges

    edges <- newSTRef $ foldMap (Seq.fromList . liftA2 (++) sameRow diffRow) builtInEdges

    let weight ps (u, v)
            = abs (colIndex u - colIndex v)
            + wt * sum [ unsafeIndex ps i | i <- [ rowIndex u + 1 .. rowIndex v - 1] ]
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

        (_, pos, (u, v)) <- ifoldl'
            (\ j (w, i, e) f -> let x = weight ps f in if x < w then (x, j, f) else (w, i, e))
            (maxBound, (-1), undefined) <$> readSTRef edges
            -- ^ find the minimum weighted edge and its position in the sequence

        modifySTRef edges $ Seq.deleteAt pos

        cyclic <- point u `equivalent` point v

        unless cyclic
          $ if rowIndex v - rowIndex u <= 1 -- rowIndex u <= rowIndex v
            then point u `union` point v
            else do

                intersections <- sequence $ generate (rowIndex v - rowIndex u - 1) $ \ i ->
                    freeEdge (net u)
                    (rowIndex u + succ i)
                    (colIndex u + div (succ i * (colIndex v - colIndex u)) (rowIndex v - rowIndex u))
                    -- ^ the built-in edges for feedthroughs

                sequence_
                  $ Vector.zipWith union
                    (point . snd <$> intersections)
                    (point . fst <$> Vector.tail intersections)
                    -- ^ the chain of feedthroughs is interconnected

                -- connect feedthroughs to every pin in the net
                Just component <- HashMap.lookup (net u) <$> readSTRef vertices
                modifySTRef edges $ mappend $ Seq.fromList
                  [ if rowIndex upperFeed > rowIndex upperComp
                    then (lowerComp, upperFeed)
                    else (lowerFeed, upperComp)
                  | (upperFeed, lowerFeed) <- toList intersections
                  , (upperComp, lowerComp) <- toList component
                  ]

                -- adjust penalties for rows containing feedthrough
                for_ intersections $ \ (i, _) -> modify penalties (+ ss ! rowIndex i ^. granularity) (rowIndex i)

                -- add feedthroughs to its net
                modifySTRef vertices $ HashMap.insertWith (<>) (net u) intersections

                -- save feedthroughs to the netgraph
                modifySTRef feedthroughs $ HashMap.insertWith (<>) (net u) intersections

    intersections <- readSTRef feedthroughs

    let locate :: Edge s -> Gate -> Gate
        locate (u, _) = space %~ relocateL (colIndex u) . relocateB (ss ! rowIndex u ^. b)

    pure $ top &~ do
        gates <>= ifoldMap (fmap . flip locate . feedthroughGate) intersections
        gates  %= imap (set number)




feedthroughGate :: Identifier -> Gate
feedthroughGate n = def &~ do
    identifier .= ("FEEDTHROUGH." <> n)
    feedthrough .= True
    wires .= HashMap.singleton "FD" n

