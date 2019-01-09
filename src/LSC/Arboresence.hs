{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LSC.Arboresence where

import Control.Monad.Trans
import Data.Default
import Data.Foldable
import Data.Map (assocs, fromAscList)
import qualified Data.Map as Map
import qualified Data.Vector as Vector
import Data.SBV
import Data.SBV.Control
import Data.Text (unpack)
import System.IO

import LSC.Types


pnr :: NetGraph -> LSC NetGraph
pnr netlist@(NetGraph name pins _ gates wires) = do

  debug
    [ "start pnr @ module", unpack name
    , "-", show $ length gates, "gates"
    , "-", show $ length wires, "nets"
    ]

  liftSMT $ do
    setOption $ ProduceUnsatCores True

  nodes <- Map.fromList <$> sequence (freeGatePolygon <$> toList gates)

  boundedSpace nodes
  collision nodes

  result <- computeStage1 nodes

  debug ["stop  pnr @ module", unpack name]

  case result of
    Nothing -> pure netlist
    Just xs -> pure netlist { gateVector = assign xs }

  where

    assign xs = Vector.fromList
      [ gate { gatePath = [rect] }
      | (gate, rect) <- toList xs
      ]



boundedSpace nodes = do

  (width, height) <- padDimensions <$> ask

  sequence_
    [ do
      liftSMT $ do
        constrain
          $   left   .> literal 0
          .&& left   .< literal width
          .&& bottom .> literal 0
          .&& bottom .< literal height

    | path <- toList nodes
    , (left, bottom) <- take 1 path
    ]


collision nodes = do
  sequence_
    [ do

      liftSMT $ do
        constrain
          $   left2 .> right1
          .|| left1 .> right2
          .|| bottom1 .> top2
          .|| bottom2 .> top1

    | (path1, path2) <- pairs $ toList nodes
    , let (left1, bottom1) : (right1, top1) : _ = path1
    , let (left2, bottom2) : (right2, top2) : _ = path2
    ]


pairs [] = []
pairs (x : xs) = fmap (x, ) xs ++ pairs xs


rows n = channels n . toList

channels n [] = pure ()
channels n nodes = do

  let (xs, rest) = splitAt n nodes

  sequence_
    [ do

      liftSMT $ do
        constrain
          $   left1 .== left2
          .&& bottom2 .> top1

    | (path1, path2) <- xs `zip` drop 1 xs
    , let (left1, _) : (_, top1) : _ = path1
    , let (left2, bottom2)  : _  : _ = path2
    ]

  sequence_
    [ do

      liftSMT $ do
        constrain $ left2 - right1 .== 8000

    | (path1, path2) <- take 1 xs `zip` take 1 rest
    , let _ : (right1, _) : _ = path1
    , let (left2, _)  : _ : _ = path2
    ]

  channels n rest


freeGatePolygon gate = do

  path @ ((left, bottom) : (right, top) : _) <- freeRectangle
  (width, height) <- lookupDimensions gate <$> ask

  liftSMT $ constrain
    $   right - left .== literal width
    .&& top - bottom .== literal height

  pure (gate, path)


freeRectangle = freePolygon 2

freePolygon n = sequence $ replicate n freePoint


freePoint = liftSMT $ (,) <$> free_ <*> free_


rectangle ((left, bottom) : (right, top) : _) = Rect
  <$> ((, ) <$> getValue left  <*> getValue bottom)
  <*> ((, ) <$> getValue right <*> getValue top)


computeStage1 nodes = do

  liftSMT $ query $ do
    result <- checkSat
    case result of

      Sat -> Just

        <$> sequence
            [ (gate, ) <$> rectangle xs
            | (gate, xs) <- assocs nodes
            ]

      Unsat -> do

        unsat <- getUnsatCore
        liftIO $ sequence_ $ hPutStrLn stderr <$> unsat

        pure Nothing

      _ -> do

        reason <- getUnknownReason
        liftIO $ hPutStrLn stderr $ show reason

        pure Nothing

