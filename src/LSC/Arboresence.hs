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

import LSC.Types


pnr :: NetGraph -> LSC Stage1
pnr (NetGraph _ pins _ gates wires) = do

  liftSMT $ do
    setOption $ ProduceUnsatCores True

  nodes <- Map.fromList <$> sequence (freeGatePolygon <$> toList gates)
  edges <- Map.fromList <$> sequence (freeWirePolygon <$> toList wires)

  boundedSpace nodes
  collision nodes
  rectilinear edges

  steiner <- arboresences nodes edges

  computeStage1 nodes edges steiner


arboresences nodes edges = do
  Map.fromAscList <$> sequence
    [ (,) net <$> arboresence nodes net path

    | (net, path) <- Map.assocs edges
    ]


arboresence nodes net path = do

  let n   = Map.size $ contacts net
      hs  = hananGrid nodes net
      ihs = hananGridIntersections hs

  if null hs
  then pure mempty

  else do

    steinerNodes <- sequence $ replicate (n - 2) freePoint

    liftSMT $ do

      constrain $ bAll (`sElem` ihs) steinerNodes
      constrain $ bnot $ bAny (`sElem` hs) steinerNodes

    pure steinerNodes


boundedSpace nodes = do
  (w, h) <- padDimensions <$> ask
  sequence_
    [ liftSMT $ do
        constrain $ x .> literal 0 &&& y .> literal 0
        constrain $ x .< literal w &&& y .< literal h
    | (x, y) <- join $ take 1 <$> Map.elems nodes
    ]


rectilinear edges = sequence_ [ rect path | path <- toList edges ]

rect ((x1, y1) : (x2, y2) : xs) = do
  liftSMT $ constrain $ x1 .== x2 ||| y1 .== y2
  rect ((x2, y2) : xs)
rect _ = pure ()


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
    [ (gx + literal px, gy + literal py)
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

  -- resolution <- wireResolution <$> ask
  let resolution = 3 * Map.size (contacts net)

  path <- freePolygon resolution

  pure (net, path)


freePolygon n = sequence $ replicate n freePoint


freePoint = liftSMT $ (,) <$> free_ <*> free_


computeStage1 nodes edges steiner = do

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

        <*> (Map.fromAscList <$> sequence
            [ fmap (net, ) $ sequence
                [ (, ) <$> getValue x <*> getValue y | (x, y) <- xs ]
            | (net, xs) <- Map.assocs steiner
            ])

      Unsat -> do

        unsat <- getUnsatCore
        liftIO $ sequence_ $ hPutStrLn stderr <$> unsat

        pure $ Circuit2D mempty mempty mempty

      _ -> do

        reason <- getUnknownReason
        liftIO $ hPutStrLn stderr $ show reason

        pure $ Circuit2D mempty mempty mempty


