{-# LANGUAGE OverloadedStrings #-}

module LSC.Exline where

import Control.Lens
import Control.Monad.ST
import Data.Default
import Data.Foldable
import Data.IntSet (IntSet, size, elems)
import Data.Map (assocs, fromList, filter, mapWithKey, lookup, restrictKeys, withoutKeys, keysSet)
import qualified Data.Set as Set
import Data.Vector (fromListN, (!))
import Prelude hiding (filter, lookup)

import LSC.FM
import LSC.NetGraph
import LSC.Types


exline :: NetGraph -> LSC NetGraph
exline = partition 3


partition :: Int -> NetGraph -> LSC NetGraph
partition 0 top = pure top
partition k top = do

  let P (p, q) = runST $ evalFM $ partitionFM top

  -- get a gate
  let g i = view gates top ! i

  -- gate vector for each partition
  let g1 = fromListN (size p) (g <$> elems p)
      g2 = fromListN (size q) (g <$> elems q)

  -- edge identifiers for each partition
  let e1 = Set.fromList $ snd <$> foldMap (view $ wires . to assocs) g1
      e2 = Set.fromList $ snd <$> foldMap (view $ wires . to assocs) g2

      eb = Set.intersection e1 e2
      ee = Set.difference eb $ top ^. supercell . pins . to keysSet

  -- signals originating in first partition
  cells <- view stdCells <$> technology
  let s1 = Set.fromList
        [ name
        | g <- toList g1
        , c <- toList $ view identifier g `lookup` cells
        , (i, name) <- g ^. wires . to assocs
        , sp <- toList $ lookup i $ c ^. pins
        , sp ^. dir == Just Out
        ]

  let fs = fromList [(e, def & identifier .~ e & dir .~ Just Out) | e <- toList ee]
      f1 = restrictKeys fs s1 <> fmap invert (withoutKeys fs s1)
      f2 = withoutKeys fs s1 <> fmap invert (restrictKeys fs s1)

  -- super cell pins for each partition
  let p1 = filter (\ x -> Set.member (x ^. identifier) e1) (top ^. supercell . pins)
      p2 = filter (\ x -> Set.member (x ^. identifier) e2) (top ^. supercell . pins)

  -- super cells for each partition
  let c1 = top &~ do
        identifier .= view identifier top <> "_1"
        supercell %= (pins .~ p1 <> f1)
        gates .= g1
        nets .= mempty
  let c2 = top &~ do
        identifier .= view identifier top <> "_2"
        supercell %= (pins .~ p2 <> f2)
        gates .= g2
        nets .= mempty

  -- wires for each new cell
  let w1 = mapWithKey (\ i _ -> i) p1
      w2 = mapWithKey (\ i _ -> i) p2

  -- new cell for each partition
  let n1 = def &~ do
        identifier .= view identifier c1
        wires .= w1
  let n2 = def &~ do
        identifier .= view identifier c2
        wires .= w2


  let result = top &~ do
        gates .= fromListN 2 [n1, n2]
        subcells .= fromList [(c1 ^. identifier, c1), (c2 ^. identifier, c2)]

  debug
      [ netGraphStats result
      , "cut size:", show $ length ee
      ]

  pure result


partitionFM :: NetGraph -> FM s Partition
partitionFM top = fiducciaMattheyses =<< inputRoutine
    (top ^. nets . to length)
    (top ^. gates . to length)
    [ (n, c)
    | (n, w) <- zip [0..] $ toList $ top ^. nets
    , (c, _) <- w ^. contacts . to assocs
    ]

  
