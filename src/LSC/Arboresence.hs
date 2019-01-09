{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LSC.Arboresence where

import Control.Monad.Trans
import Data.Default
import Data.Foldable
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

  nodes <- sequence $ freeGatePolygon <$> gates

  boundedSpace nodes
  collision nodes

  newGateVector <- computeStage1 nodes

  debug ["stop  pnr @ module", unpack name]

  pure netlist
    { gateVector = maybe (gateVector netlist) id newGateVector
    }



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

    | (_, path) <- toList nodes
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

    | ((_, path1), (_, path2)) <- distinctPairs $ toList nodes
    , let (left1, bottom1) : (right1, top1) : _ = path1
    , let (left2, bottom2) : (right2, top2) : _ = path2
    ]


distinctPairs :: [a] -> [(a, a)]
distinctPairs [] = []
distinctPairs (x : xs) = fmap (x, ) xs ++ distinctPairs xs


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


computeStage1 nodes = do

  liftSMT $ query $ do
    result <- checkSat
    case result of

      Sat -> Just

        <$> sequence (assign <$> nodes)

      Unsat -> do

        unsat <- getUnsatCore
        liftIO $ sequence_ $ hPutStrLn stderr <$> unsat

        pure Nothing

      _ -> do

        reason <- getUnknownReason
        liftIO $ hPutStrLn stderr $ show reason

        pure Nothing

  where

    assign (gate, xs) = rectangle xs >>= \ path -> pure gate { gatePath = pure path }

    rectangle ((left, bottom) : (right, top) : _) = Rect
      <$> ((, ) <$> getValue left  <*> getValue bottom)
      <*> ((, ) <$> getValue right <*> getValue top)
