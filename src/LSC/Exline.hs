{-# LANGUAGE OverloadedStrings #-}

module LSC.Exline where

import Control.Lens
import Control.Monad.ST
import Data.Foldable
import Data.IntSet (IntSet, size, elems)
import Data.Map (assocs, fromList)
import Data.Vector (fromListN, (!))

import LSC.FM
import LSC.Types


exline :: NetGraph -> LSC NetGraph
exline top = do

  let P (p, q) = runST $ evalFM $ exlineFM top

  let g i = view gates top ! i

  let c1 = top &~ do
        gates .= fromListN (size p) (g <$> elems p)
        identifier .= view identifier top <> "_1"
  let c2 = top &~ do
        gates .= fromListN (size q) (g <$> elems q)
        identifier .= view identifier top <> "_2"

  pure $ top &~ do
    gates .= mempty
    subcells .= fromList [(c1 ^. identifier, c1), (c2 ^. identifier, c2)]



exlineIO :: NetGraph -> IO (P IntSet)
exlineIO = stToIO . evalFM . exlineFM

exlineFM :: NetGraph -> FM s (P IntSet)
exlineFM top = do
  graph <- inputRoutine (top ^. nets . to length) (top ^. gates . to length)
    [ (n, c)
    | (n, net) <- zip [0..] $ toList $ top ^. nets
    , (c,   _) <- net ^. contacts . to assocs
    ]
  fiduccia graph
  
