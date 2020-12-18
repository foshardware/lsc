-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}

module LSC.GlobalRouting
    ( determineFeedthroughs
    , globalDetermineFeedthroughs
    ) where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Loops
import Control.Monad.ST
import Data.Default
import Data.Foldable
import qualified Data.HashMap.Lazy as HashMap
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Maybe
import Data.STRef
import Data.Vector ((!), fromListN, unsafeThaw, unsafeIndex, accumulate, generate)
import qualified Data.Vector as Vector
import Data.Vector.Mutable (modify)

import LSC.NetGraph
import LSC.Types
import LSC.UnionFind



wt :: Int
wt = 1


type Edge s = (Vertex s, Vertex s)


data Vertex s = Vertex
  { _rowIndex  :: Int
  ,  colIndex  :: Int
  ,  component :: Identifier
  ,  point     :: DisjointSet s
  }


rowIndex :: Vertex s -> Int
rowIndex v = _rowIndex v `div` 2
{-# INLINE rowIndex #-}


freeEdge :: Identifier -> Int -> Int -> ST s (Edge s)
freeEdge n i j = do
    (x, y) <- pair
    pure (Vertex (i * 2) j n x, Vertex (i * 2 + 1) j n y)



determineFeedthroughs :: NetGraph -> LSC NetGraph
determineFeedthroughs = liftIO . stToIO . globalDetermineFeedthroughs


-- | O(n^2) in the number of pins
--
globalDetermineFeedthroughs :: NetGraph -> ST s NetGraph
globalDetermineFeedthroughs top = do

    let rs = top ^. supercell . rows
        ss = fromListN (length rs) (toList rs)

    let row :: Gate -> Maybe Row
        row g = top ^. supercell . rows ^? ix (g ^. space . b)

    assert "globalDetermineFeedthroughs: gates are not aligned to rows!"
      $ all (isJust . row) $ top ^. gates


    let builtInEdge :: Identifier -> (Gate, Pin) -> ST s (Edge s)
        builtInEdge n (g, p) = freeEdge n (fromJust (row g) ^. number) (p ^. geometry . to (foldMap' id) . l)

    let sameRow zs = join
            [ [ (lupper, rupper), (llower, rlower) ] 
            | xs <- zs
            , ((lupper, llower), (rupper, rlower)) <- zip xs $ tail xs
            ]

    let diffRow zs =
            [ (lower, upper)
            | (xs, ys) <- distinctPairs zs
            , (_, lower) <- xs
            , (upper, _) <- ys
            ]

    builtInEdges <- views nets forM top $ \ net ->
        views identifier (mapM . builtInEdge) net `mapM` verticesByRow top net
        -- ^ the built-in edges for all pins in the layout grouped by net and row

    vertices <- newSTRef $ foldMap Vector.fromList <$> builtInEdges

    edges <- newSTRef $ foldMap (\ xs -> Seq.fromList $ sameRow xs ++ diffRow xs) builtInEdges

    let weight ps (u, v)
            = abs (colIndex u - colIndex v)
            + wt * sum [ unsafeIndex ps i | i <- [ rowIndex u + 1 .. rowIndex v - 1] ]
            -- rowIndex u <= rowIndex v

    penalties <- unsafeThaw
      $ accumulate (+) (Vector.replicate (length rs) 0)
      $ Vector.zip
        (view number . fromJust . row <$> view gates top)
        (gateWidth <$> view gates top)

    feedthroughs <- newSTRef mempty

    whileM_ (not . null <$> readSTRef edges)
      $ do

        ps <- Vector.freeze penalties

        (_, (pos, (u, v))) <- foldlWithIndex'
              (\ (w, (p, e)) q f -> let x = weight ps f in if x < w then (x, (q, f)) else (w, (p, e)))
              (maxBound, ((-1), undefined))
              <$> readSTRef edges -- find the minimum weighted edge and its position in the sequence

        modifySTRef edges $ Seq.deleteAt pos

        cyclic <- equivalent (point u) (point v)

        unless cyclic
          $ if rowIndex v - rowIndex u <= 1 -- rowIndex u <= rowIndex v
            then union (point u) (point v)
            else do

                intersections <- sequence $ generate (rowIndex v - rowIndex u - 1) $ \ i ->
                    freeEdge (component u)
                    (rowIndex u + succ i)
                    (colIndex u + div (succ i * (colIndex v - colIndex u)) (rowIndex v - rowIndex u))
                    -- ^ the built-in edges for feedthroughs

                sequence_
                  $ Vector.zipWith union
                    (point . snd <$> intersections)
                    (point . fst <$> Vector.tail intersections)
                    -- ^ the chain of feedthroughs is interconnected

                -- connect feedthroughs to every pin in the component
                Just xs <- HashMap.lookup (component u) <$> readSTRef vertices
                modifySTRef edges $ mappend $ foldMap id
                    [ case rowIndex upperFeed `compare` rowIndex upperComp of
                        GT -> pure (lowerComp, upperFeed)
                        LT -> pure (lowerFeed, upperComp)
                        EQ -> pure (upperFeed, upperComp) <> pure (lowerFeed, lowerComp)
                    | (upperFeed, lowerFeed) <- toList intersections
                    , (upperComp, lowerComp) <- toList xs
                    ]

                -- adjust penalties for rows containing feedthrough
                for_ intersections $ \ (i, _) -> modify penalties (+ ss ! rowIndex i ^. granularity) (rowIndex i)

                -- add feedthroughs to its component
                modifySTRef vertices $ HashMap.insertWith (<>) (component u) intersections

                -- save feedthroughs to the netgraph
                modifySTRef feedthroughs $ HashMap.insertWith (<>) (component u) intersections

    intersections <- readSTRef feedthroughs

    let locate :: Edge s -> Gate -> Gate
        locate (u, _) = space %~ relocateL (colIndex u) . relocateB (ss ! rowIndex u ^. b)

    pure $ top &~ do
        gates <>= HashMap.foldMapWithKey (fmap . flip locate . feedthroughGate) intersections
        gates  %= imap (set number)




feedthroughGate :: Identifier -> Gate
feedthroughGate net = def &~ do
    identifier .= ("FEEDTHROUGH." <> net)
    feedthrough .= True
    wires .= HashMap.singleton "FD" net



foldlWithIndex' :: (b -> Int -> a -> b) -> b -> Seq a -> b
foldlWithIndex' f y xs = foldl' (\ g x !i -> f (g (i - 1)) i x) (const y) xs (length xs - 1)
{-# INLINE foldlWithIndex' #-}

