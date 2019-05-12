
module LSC.Easy where

import Control.Applicative
import Control.Lens
import Control.Monad.State
import Data.Default
import Data.Foldable
import Data.Map (Map, lookup)
import Data.Maybe
import Data.Text (unpack)
import Prelude hiding (lookup)

import LSC.Types


placeEasy :: NetGraph -> LSC NetGraph
placeEasy netlist = do

  offset <- (4 *) . fst . view standardPin <$> technology
  rows  <- fmap (+ offset) <$> divideArea (netlist ^. gates)

  let pivot = div (netlist ^. gates & length & succ) (length rows)

  let abstractCells = maybe (0,0) (\ p -> (p^.r, p^.t))
        . listToMaybe . view geometry . view supercell <$> view subcells netlist

  nodes <- evalStateT
    (sequence $ netlist ^. gates <&> sections abstractCells)
    (offset, offset, replicate pivot =<< alternate rows)

  let x = maximum $ view r . head . view geometry <$> nodes
      y = maximum $ view t . head . view geometry <$> nodes

  debug [ unpack (view identifier netlist) ++ " layout area: " ++ show (x + offset, y + offset) ]

  let super = def &~ do
        geometry .= [Rect 0 0 (x + offset) (y + offset)]

  pure $ netlist &~ do
      gates .= nodes
      supercell .= super


alternate :: [a] -> [Either a a]
alternate (x : y : xs) = Right x : Left y : alternate xs
alternate (x : _) = [Right x]
alternate _ = []


sections
  :: Map Identifier (Integer, Integer)
  -> Gate
  -> StateT (Integer, Integer, [Either Integer Integer]) LSC Gate
sections subs gate = do

  offset <- (4 *) . fst . view standardPin <$> lift technology

  let rotate (x, y) = if x > 2 * y then (y, x) else (x, y)

  tech <- lift technology
  let (w, h) = maybe (0, 0) rotate $ lookup (view identifier gate) subs <|> lookupDims gate tech

  (x, y, rs) <- get

  case rs of

    [] -> pure gate

    Right _ : Left next : rows -> do
      put (x + w + offset, y + h + offset, Left next : rows)
      pure $ gate &~ do
          geometry .= [Layered x y (x + w) (y + h) [Metal2, Metal3]]

    Left _ : Right next : rows -> do
      put (x + w + offset, y - h - offset, Right next : rows)
      pure $ gate &~ do
          geometry .= [Layered x (y - h - offset) (x + w) (y - offset) [Metal2, Metal3]]

    Left _ : rows -> do
      put (x, y - h - offset, rows)
      pure $ gate &~ do
          geometry .= [Layered x (y - h - offset) (x + w) (y - offset) [Metal2, Metal3]]

    Right _ : rows -> do
      put (x, y + h + offset, rows)
      pure $ gate &~ do
          geometry .= [Layered x y (x + w) (y + h) [Metal2, Metal3]]

