{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module LSC.Exline where

import Control.Lens
import Control.Monad.IO.Class
import Data.Default
import Data.Foldable hiding (concat)
import Data.Function
import Data.IntSet (size, elems)
import Data.Map hiding (size, elems, toList, null, (!))
import qualified Data.Set as Set
import Data.Text (unpack)
import Data.Vector (fromListN, (!))
import Prelude hiding (filter, concat, lookup)

import LSC.FM as FM
import LSC.NetGraph
import LSC.Types



exline :: NetGraph -> LSC NetGraph
exline = recursiveBisection 4


recursiveBisection :: Int -> NetGraph -> LSC NetGraph
recursiveBisection i top
  | top ^. gates . to length < i
  = pure top
recursiveBisection i top = do
    next <- bisection top
    if next ^. subcells . to length <= 1
        then pure top
        else flip (set subcells) next <$> recursiveBisection i `mapM` view subcells next


bisection :: NetGraph -> LSC NetGraph
bisection top = do

  debug
    [ unpack (top ^. identifier) ++ ": exlining starts"
    , netGraphStats top
    ]

  solution <- liftIO $ solutionVectorOf 16 $ do
      h <- st $ inputRoutine
          (top ^. nets . to length)
          (top ^. gates . to length)
          [ (n, c)
          | (n, w) <- zip [0..] $ toList $ top ^. nets
          , (c, _) <- w ^. contacts . to assocs
          ]
      (h, ) <$> fmMultiLevel h coarseningThreshold matchingRatio

  let criterion (h, x) = bisectBalance x + 2 * cutSize h x
      Bisect p q = snd $ minimumBy (compare `on` criterion) solution

  -- get a gate
  let g i = view gates top ! i

  -- gate vector for each partition
  let g1 = set number `imap` fromListN (size p) (g <$> elems p)
      g2 = set number `imap` fromListN (size q) (g <$> elems q)

  -- edge identifiers for each partition
  let e1 = Set.fromList $ snd <$> foldMap (view $ wires . to assocs) g1
      e2 = Set.fromList $ snd <$> foldMap (view $ wires . to assocs) g2

      eb = Set.intersection e1 e2
      cut = Set.difference eb $ top ^. supercell . pins . to keysSet

  -- signals originating in first partition
  cells <- view stdCells <$> technology
  let s1 = Set.fromList
        [ name
        | n <- toList g1
        , c <- toList $ view identifier n `lookup` cells
        , (i, name) <- n ^. wires . to assocs
        , sp <- toList $ lookup i $ c ^. pins
        , sp ^. dir == Just Out
        ]

  -- cut edges between partitions
  let fc = fromList [(e, def & identifier .~ e & dir .~ Just Out) | e <- toList cut]
      f1 = mconcat [restrictKeys fc s1, invert <$> withoutKeys fc s1]
      f2 = mconcat [withoutKeys fc s1, invert <$> restrictKeys fc s1]

  -- super cell pins for each partition
  let p1 = restrictKeys (top ^. supercell . pins) e1
      p2 = restrictKeys (top ^. supercell . pins) e2

  -- super cells for each partition
  let c1 = top &~ do
        identifier .= view identifier top <> "_"
        supercell %= (pins .~ p1 <> f1)
        gates .= g1
        nets .= rebuildEdges g1
  let c2 = top &~ do
        identifier .= view identifier top <> "-"
        supercell %= (pins .~ p2 <> f2)
        gates .= g2
        nets .= rebuildEdges g2

  -- new cell for each partition
  let n1 = def &~ do
        identifier .= view identifier c1
        wires .= mapWithKey const p1
  let n2 = def &~ do
        identifier .= view identifier c2
        wires .= mapWithKey const p2

  let pt = [(n1, c1) | size p > 0] ++ [(n2, c2) | size q > 0]

  let result = top &~ do
        gates .= (fst <$> fromListN 2 pt)
        nets .= rebuildEdges (fst <$> fromListN 2 pt)
        subcells .= fromList (fmap (\ (_, c) -> (view identifier c, c)) pt)

  debug
    [ unpack (view identifier top) ++ ": exlining finished"
    , netGraphStats result
    -- , "iterations: "++ show it
    , "cut size: "++ show (length cut)
    , ""
    ]

  pure result


