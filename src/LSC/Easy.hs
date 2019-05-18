
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



placeRows :: NetGraph -> LSC NetGraph
placeRows top = do
 
  (gs, (x, y)) <- runStateT (sequence $ top ^. gates <&> afterRow top) (0, 0)

  pure $ top &~ do
    gates .= gs
    supercell %= (geometry .~ [Rect 0 0 x y])


afterRow :: NetGraph -> Gate -> StateT (Integer, Integer) LSC Gate
afterRow top g
  | Just sub <- lookup (g ^. identifier) (top ^. subcells)
  , Rect 0 0 w h : _ <- sub ^. supercell . geometry
  = do
    (x, y) <- get
    channel <- view rowSize <$> lift technology
    put (max w x, y + h + channel)
    pure $ g & geometry .~ [Layered 0 y w (y + h) [Metal2, Metal3] def]
afterRow _ g = pure g



placeColumn :: NetGraph -> LSC NetGraph
placeColumn netlist = do

  (gs, (x, y)) <- runStateT (sequence $ netlist ^. gates <&> afterColumn) (0, 0)

  pure $ netlist &~ do
    gates .= gs 
    supercell %= (geometry .~ [Rect 0 0 x y])


afterColumn :: Gate -> StateT (Integer, Integer) LSC Gate
afterColumn g = do
    (x, y) <- get
    ds <- lookupDims g <$> lift technology
    case ds of
      Just (w, h) -> do
        put (x + w + 2000, max y h)
        pure $ g & geometry .~ [Layered x 0 (x + w) h [Metal2, Metal3] def]
      _ -> pure g


placeEasy :: NetGraph -> LSC NetGraph
placeEasy netlist = do

  off <- (4 *) . fst . view standardPin <$> technology
  rows  <- fmap (+ off) <$> divideArea (netlist ^. gates)

  let pivot = div (netlist ^. gates & length & succ) (length rows)

  let abstractCells = maybe (0,0) (\ p -> (p^.r, p^.t))
        . listToMaybe . view geometry . view supercell <$> view subcells netlist

  nodes <- evalStateT
    (sequence $ netlist ^. gates <&> sections abstractCells)
    (off, off, replicate pivot =<< alternate rows)

  let x = maximum $ maybe 0 (view r) . listToMaybe . view geometry <$> nodes
      y = maximum $ maybe 0 (view t) . listToMaybe . view geometry <$> nodes

  debug [ unpack (view identifier netlist) ++ " layout area: " ++ show (x + off, y + off) ]

  let super = def &~ do
        geometry .= [Rect 0 0 (x + off) (y + off)]

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
  -> StateT (Integer, Integer, [Either a a]) LSC Gate
sections subs gate = do

  off <- (4 *) . fst . view standardPin <$> lift technology

  let rotate (x, y) = if x > y then (y, x) else (x, y)

  tech <- lift technology
  let (w, h) = maybe (0, 0) rotate $ lookup (view identifier gate) subs <|> lookupDims gate tech

  (x, y, rs) <- get

  case rs of

    [] -> pure gate

    Right _ : Left next : rows -> do
      put (x + w + off, y + h + off, Left next : rows)
      pure $ gate &~ do
          geometry .= [Layered x y (x + w) (y + h) [Metal2, Metal3] def]

    Left _ : Right next : rows -> do
      put (x + w + off, y - h - off, Right next : rows)
      pure $ gate &~ do
          geometry .= [Layered x (y - h - off) (x + w) (y - off) [Metal2, Metal3] def]

    Left _ : rows -> do
      put (x, y - h - off, rows)
      pure $ gate &~ do
          geometry .= [Layered x (y - h - off) (x + w) (y - off) [Metal2, Metal3] def]

    Right _ : rows -> do
      put (x, y + h + off, rows)
      pure $ gate &~ do
          geometry .= [Layered x y (x + w) (y + h) [Metal2, Metal3] def]

