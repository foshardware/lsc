{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE GADTs, DataKinds, TupleSections, FlexibleContexts #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LSC.Arboresence where

import Control.Monad
import Control.Monad.Trans
import Data.Foldable
import qualified Data.Map as Map
import Data.SBV
import Data.SBV.Control
import System.IO

import LSC.NetGraph
import LSC.Types
import LSC.Exlining


type Stage1 = Circuit2D

stage1 :: Int -> NetGraph -> LSC Stage1
stage1 j
  = fmap head
  . concLSC
  . take j
  . fmap pnr
  . getLeaves
  . exline (repeat 20)


pnr :: NetGraph -> LSC Circuit2D
pnr (NetGraph _ pins _ gates wires) = do

  liftSMT $ do
    setOption $ ProduceUnsatCores True

  nodes <- Map.fromList <$> sequence (freeGatePolygon <$> toList gates)
  edges <- Map.fromList <$> sequence (freeWirePolygon <$> toList wires)

  liftSMT $ query $ do
    result <- checkSat
    case result of

      Sat -> Circuit2D

        <$> sequence
            [ fmap (gate, ) $ Path <$> sequence
                [ (, ) <$> getValue x <*> getValue y | (x, y) <- path ]
            | (gate, path) <- Map.assocs nodes
            ]

        <*> sequence
            [ fmap (net, ) $ Path <$> sequence
                [ (, ) <$> getValue x <*> getValue y | (x, y) <- path ]
            | (net, path) <- Map.assocs edges
            ]

      Unsat -> do

        unsat <- getUnsatCore
        liftIO $ sequence_ $ hPutStrLn stderr <$> unsat

        pure $ Circuit2D [] []

      _ -> do

        reason <- getUnknownReason
        liftIO $ hPutStrLn stderr $ show reason

        pure $ Circuit2D [] []


freeGatePolygon gate = do

  (width, height) <- lookupDimensions gate <$> ask

  path <- freePolygon 4

  let (x1, y1) : (x2, y2) : (x3, y3) : (x4, y4) : _ = path

  liftSMT $ constrain
    $   y2 - y1 .== literal height
    &&& y3 - y4 .== literal height
    &&& x3 - x2 .== literal width
    &&& x4 - x1 .== literal width  

  pure (gate, path)


freeWirePolygon net = do

  resolution <- wireResolution <$> ask

  path <- freePolygon resolution

  pure (net, path)


freePolygon n = sequence $ replicate n freePoint


freePoint = liftSMT $ (,) <$> free_ <*> free_

