{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE GADTs, DataKinds, TupleSections, FlexibleContexts #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LSC.Arboresence where

import Control.Monad
import Control.Monad.Trans
import Data.Foldable
import qualified Data.Map as Map
import Data.Monoid
import Data.SBV
import Data.SBV.Control
import System.IO

import LSC.Types


pnr :: NetGraph -> LSC Stage1
pnr (NetGraph _ pins _ gates wires) = do

  liftSMT $ do
    setOption $ ProduceUnsatCores True

  nodes <- Map.fromList <$> sequence (freeGatePolygon <$> toList gates)

  boundedSpace nodes
  collision nodes

  steiner <- arboresence pins nodes wires

  rectilinear steiner

  computeStage1 nodes steiner


arboresence pins nodes wires = Map.unions <$> sequence (arbor pins nodes <$> toList wires)

arbor pins@(inputs, outputs, _) nodes net = Map.fromList <$> sequence
    [ do

      source@(sx, _) <- freePoint

      when (netIdent net `elem` inputs) $ liftSMT $ do
        constrain $ bAnd [ sx .< x | (x, _) : _ <- toList nodes ]

      edge <- branch net gate pin source

      pure ((net, idn), edge)

    | (gate, cs) <- Map.assocs (contacts net)
                 <> mempty
    , (idn, pin) <- cs
    ]


branch net gate pin source = do

    target <- freePoint

    (_, edge) <- freeWirePolygon net

    liftSMT $ do
      constrain $ head edge .== source
      constrain $ last edge .== target

    pure edge


manhattan :: (SInteger, SInteger) -> (SInteger, SInteger) -> SInteger
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)


boundedSpace nodes = do
  (w, h) <- padDimensions <$> ask
  sequence_
    [ liftSMT $ do
        constrain $ x .> literal 0 &&& y .> literal 0
        constrain $ x .< literal w &&& y .< literal h
    | rect <- Map.elems nodes
    , (x, y) <- take 1 rect
    ]


rectilinear steiner = sequence_ [ rectangular edge | edge <- toList steiner ]

rectangular ((x1, y1) : (x2, y2) : xs) = do
  liftSMT $ constrain $ x1 .== x2 ||| y1 .== y2
  rectangular ((x2, y2) : xs)
rectangular _ = pure ()


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


hananGridIntersections s = [ (x, y) | (x, _) <- s, (_, y) <- s ]

hananGrid nodes net =
    [ (pinDir pin, (gx + literal px, gy + literal py))
    | (gate, cs) <- Map.assocs $ contacts net
    , (_, pin) <- take 1 cs
    , (px, py, _, _) <- take 1 $ portRects $ pinPort pin
    , (gx, gy) <- maybe [] (take 1) (Map.lookup gate nodes)
    ]


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


computeStage1 nodes steiner = do

  liftSMT $ query $ do
    result <- checkSat
    case result of

      Sat -> Circuit2D

        <$> sequence
            [ fmap (gate, ) $ Path <$> sequence
                [ (, ) <$> getValue x <*> getValue y | (x, y) <- xs ]
            | (gate, xs) <- Map.assocs nodes
            ]

        <*> sequence
            [ fmap (net, ) $ Path <$> sequence
                [ (, ) <$> getValue x <*> getValue y | (x, y) <- xs ]
            | (net, xs) <- Map.assocs steiner
            ]

      Unsat -> do

        unsat <- getUnsatCore
        liftIO $ sequence_ $ hPutStrLn stderr <$> unsat

        pure $ Circuit2D mempty mempty

      _ -> do

        reason <- getUnknownReason
        liftIO $ hPutStrLn stderr $ show reason

        pure $ Circuit2D mempty mempty


