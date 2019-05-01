{-# LANGUAGE OverloadedStrings #-}

module LSC.Exline where

import Control.Lens
import Control.Monad.ST
import Data.Default
import Data.Foldable
import Data.IntSet (IntSet, size, elems)
import Data.Map (assocs, fromList, filter, mapWithKey)
import qualified Data.Set as Set
import Data.Vector (fromListN, (!))
import Prelude hiding (filter)

import LSC.FM
import LSC.Types


exline :: NetGraph -> LSC NetGraph
exline top = do

  let P (p, q) = runST $ evalFM $ exlineFM top

  let g i = view gates top ! i

  let g1 = fromListN (size p) (g <$> elems p)
      g2 = fromListN (size q) (g <$> elems q)

  let e1 = foldMap (^. wires . to assocs . to (fmap snd) . to Set.fromList) g1
      e2 = foldMap (^. wires . to assocs . to (fmap snd) . to Set.fromList) g2

  let p1 = filter (flip Set.member e1 . view identifier) (top ^. supercell . pins)
      p2 = filter (flip Set.member e2 . view identifier) (top ^. supercell . pins)

  let c1 = top &~ do
        supercell %= (pins .~ p1)
        gates .= g1
        nets .= mempty
        identifier .= view identifier top <> "_1"
  let c2 = top &~ do
        supercell %= (pins .~ p2)
        gates .= g2
        nets .= mempty
        identifier .= view identifier top <> "_2"

  let w1 = mapWithKey (\ i _ -> i) p1
      w2 = mapWithKey (\ i _ -> i) p2

  let n1 = def &~ do
        identifier .= view identifier c1
        wires .= w1
  let n2 = def &~ do
        identifier .= view identifier c2
        wires .= w2

  pure $ top &~ do
    gates .= fromListN 2 [n1, n2]
    subcells .= fromList [(c1 ^. identifier, c1), (c2 ^. identifier, c2)]


exlineIO :: NetGraph -> IO Partition
exlineIO = stToIO . evalFM . exlineFM


exlineFM :: NetGraph -> FM s Partition
exlineFM top = do
  graph <- inputRoutine (top ^. nets . to length) (top ^. gates . to length)
    [ (n, c)
    | (n, net) <- zip [0..] $ toList $ top ^. nets
    , (c,   _) <- net ^. contacts . to assocs
    ]
  fiduccia graph
  
