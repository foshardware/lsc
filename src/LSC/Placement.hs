
module LSC.Placement where

import Control.Lens
import Control.Monad.State

import LSC.Types


placeEasy :: NetGraph -> LSC NetGraph
placeEasy netlist = do

  offset <- (4 *) . fst . view standardPin <$> ask
  rows  <- fmap (+ offset) . init . divideArea (netlist ^. gates) <$> ask

  let pivot = div (netlist ^. gates & length & succ) (length rows)

  nodes <- evalStateT
    (sequence $ netlist ^. gates <&> sections)
    (offset, replicate pivot =<< alternate rows)

  pure $ netlist
    & gates .~ nodes


alternate :: [a] -> [Either a a]
alternate (x : y : xs) = Right x : Left y : alternate xs
alternate (x : _) = [Right x]
alternate _ = []


sections :: Gate -> StateT (Integer, [Either Integer Integer]) LSC Gate
sections gate = do

  offset <- (2 *) . fst . view standardPin <$> lift ask

  (w, h) <- maybe (0, 0) id . lookupDimensions gate <$> lift ask

  (y, row : rows) <- get

  case row of

    Left x -> do

      put (y - h - offset, rows)

      pure $ gate
        & geometry .~ [Layered x (y - h) (x + w) y [Metal3, Metal2]]

    Right x -> do

      put (y + h + offset, rows)

      pure $ gate
        & geometry .~ [Layered x y (x + w) (y + h) [Metal3, Metal2]]

