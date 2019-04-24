
module LSC.Easy where

import Control.Lens
import Control.Monad.State
import Data.Foldable

import LSC.Types


placeEasy :: NetGraph -> LSC NetGraph
placeEasy netlist = do

  offset <- (4 *) . fst . view standardPin <$> technology
  rows  <- fmap (+ offset) <$> divideArea (netlist ^. gates)

  let pivot = div (netlist ^. gates & length & succ) (length rows) + 3

  nodes <- evalStateT
    (sequence $ netlist ^. gates <&> sections)
    (offset, replicate pivot =<< alternate rows)

  debug [ show x | x <- toList nodes, x ^. geometry . to null ]

  pure $ netlist
    & gates .~ nodes


alternate :: [a] -> [Either a a]
alternate (x : y : xs) = Right x : Left y : alternate xs
alternate (x : _) = [Right x]
alternate _ = []


sections :: Gate -> StateT (Integer, [Either Integer Integer]) LSC Gate
sections gate = do

  offset <- (2 *) . fst . view standardPin <$> lift technology

  (w, h) <- maybe (0, 0) id . lookupDimensions gate <$> lift technology

  (y, rs) <- get

  case rs of

    [] -> pure gate

    Left x : rows -> do

      put (y - h - offset, rows)

      pure $ gate
        & geometry .~ [Layered x (y - h) (x + w) y [Metal2, Metal3]]

    Right x : rows -> do

      put (y + h + offset, rows)

      pure $ gate
        & geometry .~ [Layered x y (x + w) (y + h) [Metal2, Metal3]]

