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

  collision nodes

  arboresence nodes edges

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


arboresence nodes edges = do
  sequence_
    [ do

        pure $ hananGridIntersections nodes sinks

        pure ()

    | (net, path) <- Map.assocs edges
    , let sinks = Map.assocs $ contacts net

    -- , (gate, cs) <- Map.assocs $ contacts net
    -- , (_,   pin) <- take 1 cs
    -- , (tx, ty, _, _) <- take 1 $ portRects $ pinPort pin
    -- , (x2, y2) <- maybe [] (take 1) (Map.lookup gate nodes)
    -- , let target = (x2 + literal tx, y2 + literal ty)
    ]


collision nodes = do
  sequence_
    [ do

      liftSMT $ do
        constrain $ fst topLeft1 .> fst bottomRight2 ||| fst topLeft2 .> fst bottomRight1
        constrain $ snd topLeft1 .< snd bottomRight2 ||| snd topLeft2 .< snd bottomRight1

    | (i, path1) <- [ 1 .. ] `zip` Map.elems nodes
    ,     path2  <- i `drop` Map.elems nodes
    , let _ : topLeft1 : _ : bottomRight1 : _ = path1
    , let _ : topLeft2 : _ : bottomRight2 : _ = path2
    ]


hananGridIntersections nodes sinks =

    [ (gx + literal px, gy + literal py)

    | (gate1, cs1) <- sinks
    , (gx, _) <- maybe [] (take 1) (Map.lookup gate1 nodes)
    , (_, pin1) <- take 1 cs1
    , (px, _, _, _) <- take 1 $ portRects $ pinPort pin1

    , (gate2, cs2) <- sinks
    , (_, gy) <- maybe [] (take 1) (Map.lookup gate2 nodes)
    , (_, pin2) <- take 1 cs2
    , (_, py, _, _) <- take 1 $ portRects $ pinPort pin2

    ]


freeSteinerNodes net sinks = pure ()


freeGatePolygon gate = do

  (width, height) <- lookupDimensions gate <$> ask

  path <- freePolygon 4

  let bottomLeft : topLeft : topRight : bottomRight : _ = path

  liftSMT $ constrain
    $   snd topLeft - snd bottomLeft .== literal height
    &&& fst topLeft .== fst bottomLeft

    &&& snd topRight - snd bottomRight .== literal height
    &&& fst topRight .== fst bottomRight

    &&& fst topRight - fst topLeft .== literal width
    &&& snd topRight .== snd topLeft

    &&& fst bottomRight - fst bottomLeft .== literal width
    &&& snd bottomRight .== snd bottomLeft

  pure (gate, path)


freeWirePolygon net = do

  resolution <- wireResolution <$> ask

  path <- freePolygon resolution

  pure (net, path)


freePolygon n = sequence $ replicate n freePoint


freePoint = liftSMT $ (,) <$> free_ <*> free_

