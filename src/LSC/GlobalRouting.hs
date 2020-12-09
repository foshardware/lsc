{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module LSC.GlobalRouting where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.ST
import Control.Monad.Writer
import Data.Default
import Data.Foldable
import Data.Function
import Data.Hashable
import qualified Data.HashMap.Lazy as HashMap
import Data.Semigroup
import Data.Maybe
import Data.STRef
import Data.Vector ((!), fromListN, unsafeThaw, accumulate, generate)
import qualified Data.Vector as Vector
import Data.Vector.Mutable (write, modify)
import Prelude hiding (read, lookup)

import LSC.NetGraph
import LSC.Types
import LSC.UnionFind



wt :: Int
wt = 2


type Vertex = (Int, Int, Identifier)

type Edge = (Vertex, Vertex)


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

    let builtInEdge :: Net -> (Gate, Pin) -> Edge
        builtInEdge n (g, p)
            = ((rowIndex * 2, colIndex, n ^. identifier), (rowIndex * 2 + 1, colIndex, n ^. identifier))
            where rowIndex = fromJust (row g) ^. number
                  colIndex = p ^. geometry . to (foldMap' id) . l

    let rowDiff (u, _, _) (v, _, _) = div v 2 - div u 2 -- row(u) <= row(v)


    let builtInEdgesByRow net = fmap (builtInEdge net) <$> verticesByRow top net

    let sameRow net =
            [ [ (lupper, rupper), (llower, rlower) ] 
            | xs <- builtInEdgesByRow net
            , ((lupper, llower), (rupper, rlower)) <- zip xs $ tail xs
            ]

    let diffRow net =
            [ (lower, upper)
            | (x, y) <- distinctPairs $ builtInEdgesByRow net
            , (_, lower) <- x
            , (upper, _) <- y
            ]


    let edges = Vector.fromList $ foldMap
            (\ n -> join (builtInEdgesByRow n) ++ join (sameRow n) ++ diffRow n)
            (view nets top)

    graph <- unsafeThaw $ Just <$> edges

    disjoint <- newDisjointSetSized $ 2 * sum (views members length <$> view nets top)

    penalties <- unsafeThaw
        $ accumulate (+) (Vector.replicate (length rs) 0)
        $ Vector.zip
          (view number . fromJust . row <$> view gates top)
          (gateWidth <$> view gates top)

    feedthroughs <- newSTRef mempty


    let weight ps ((u1, u2, _), (v1, v2, _))
            = abs (u2 - v2)
            + wt * sum [ ps ! i | i <- [div u1 2 + 1 .. div v1 2 - 1] ] -- row(u) <= row(v)

    for_ edges $ \ _ -> do

        ps <- Vector.freeze penalties

        (pos, Just (u, v)) <- minimumOnST (foldMap $ Min . weight ps) graph

        write graph pos Nothing

        let intersections
                = generate (rowDiff u v - 1)
                $ \ i -> ( div (u^._1) 2 + succ i
                         , u^._2 + div (succ i * (v^._2 - u^._2)) (rowDiff u v)
                         ) -- row(u) <= row(v)

        cyclic <- equivalent disjoint (hash u) (hash v)

        unless cyclic
          $ do

            for_ intersections $ \ (i, _) -> modify penalties (+ ss!i ^. granularity) i

            modifySTRef feedthroughs $ HashMap.insertWith (<>) (u^._3) intersections

            union disjoint (hash u) (hash v)

    intersections <- readSTRef feedthroughs

    let locate :: (Int, Int) -> Gate -> Gate
        locate (i, j) = space %~ relocateL j . relocateB (ss ! i ^. b)

    pure $ top &~ do
        gates <>= HashMap.foldMapWithKey (fmap . flip locate . feedthroughGate) intersections
        gates  %= imap (set number)




feedthroughGate :: Identifier -> Gate
feedthroughGate net
    = def
    & identifier .~ ("FEEDTHROUGH." <> net)
    & feedthrough .~ True
    & wires .~ HashMap.singleton "FD" net


