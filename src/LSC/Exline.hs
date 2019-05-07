{-# LANGUAGE OverloadedStrings #-}

module LSC.Exline where

import Control.Lens
import Control.Monad.IO.Class
import Data.Default
import Data.Foldable
import Data.IntSet (size, elems)
import Data.Map hiding (size, elems, toList, null, (!))
import qualified Data.Set as Set
import Data.Text (unpack)
import Data.Vector (fromListN, (!))
import Prelude hiding (filter, lookup)

import LSC.FM as FM
import LSC.NetGraph
import LSC.Types



exline :: NetGraph -> LSC NetGraph
exline top = do

  next <- recursiveBisection 16 10 top

  let ls = fromList [ (x ^. identifier, x) | x <- leaves next ]

  let gs = set number `imap` fromListN (length ls)
        [ def & identifier .~ i & wires .~ w
        | (i, x) <- assocs ls
        , let w = fromAscList [ (y, y) | y <- x ^. supercell . pins . to keys ]
        ]

  let result = next &~ do
        gates .= gs
        nets .= rebuildEdges gs
        subcells .= ls

  debug
    [ unpack (top ^. identifier) ++ ": final stats"
    , netGraphStats result
    ]

  pure result



recursiveBisection :: Int -> Int -> NetGraph -> LSC NetGraph
recursiveBisection d i top
  | d <= 0 || top ^. gates . to length < i = pure top
recursiveBisection d i top = do
  next <- bisection top
  subs <- recursiveBisection (pred d) i `mapM` view subcells next
  pure $ next &~ do
    subcells .= filter (not . null . view gates) subs


bisection :: NetGraph -> LSC NetGraph
bisection top = do

  debug
    [ unpack (top ^. identifier) ++ ": exlining starts"
    , netGraphStats top
    ]

  (Bi p q, it) <- liftIO $ nonDeterministic $ do

      h <- inputRoutine
        (top ^. nets . to length)
        (top ^. gates . to length)
        [ (n, c)
        | (n, w) <- zip [0..] $ toList $ top ^. nets
        , (c, _) <- w ^. contacts . to assocs
        ]

      (,) <$> fiducciaMattheyses h <*> value FM.iterations

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
      f1 = restrictKeys fc s1 <> fmap invert (withoutKeys fc s1)
      f2 = withoutKeys fc s1 <> fmap invert (restrictKeys fc s1)

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
        nets .= rebuildEdges (fromListN 2 [n1, n2])
        subcells .= fromList [(c1 ^. identifier, c1), (c2 ^. identifier, c2)]

  debug
    [ unpack (view identifier top) ++ ": exlining finished"
    , netGraphStats result
    , "iterations: "++ show it
    , "cut size: "++ show (length cut)
    , ""
    ]

  pure result


