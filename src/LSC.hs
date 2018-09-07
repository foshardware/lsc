{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE GADTs, DataKinds, TupleSections, FlexibleContexts #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LSC where

import Data.Foldable
import qualified Data.Map as Map

import Data.SBV
import Data.SBV.Control

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

  nodes <- Map.fromList <$> sequence (freeNode <$> toList gates)
  steiner <- Map.fromList <$> sequence (freeSteiner pins <$> toList wires)

  edges <- connect nodes wires steiner

  collision nodes
  boundedSpace nodes

  rectangular edges

  outerRim steiner nodes

  -- intersections edges

  liftSMT $ query $ do
    result <- checkSat
    case result of

      Sat -> Circuit2D

        <$> sequence
            [ fmap (gate, ) $ (,,,) <$> getValue x <*> getValue y <*> getValue w <*> getValue h
            | (gate, (x, y, w, h)) <- Map.assocs nodes
            ]

        <*> sequence
            [ fmap (net, ) $ Path <$> sequence
                [ (, ) <$> getValue x <*> getValue y | (x, y) <- path ]
            | (net, path) <- edges
            ]

      _   -> pure $ Circuit2D [] []


boundedSpace nodes = do
  (width, height) <- padDimensions <$> ask
  sequence_
    [ liftSMT $ constrain
        $   x .> literal 0
        &&& y .> literal 0
        &&& x .< literal width
        &&& y .< literal height
    | (x, y, _, _) <- Map.elems nodes
    ]


collision nodes = do
  sequence_
    [ liftSMT $ constrain
        $   x1 .> x2 &&& x1 - x2 .> w2
        ||| x2 .> x1 &&& x2 - x1 .> w1
        ||| y1 .> y2 &&& y1 - y2 .> h2
        ||| y2 .> y1 &&& y2 - y1 .> h1
    | (i, (x1, y1, w1, h1)) <- zip [ 1 .. ] $ Map.elems nodes
    ,     (x2, y2, w2, h2)  <- drop i $ Map.elems nodes
    ]


connect nodes wires steiner = do

  sequence
    [ do
      path <- freeEdge

      liftSMT $ constrain
        $   abs (fst source - fst target) + abs (snd source - snd target) .== distance
        &&& source .== head path
        &&& target .== last path

      pure (wire, path)

    | wire <- toList wires
    , (distance, _, source) <- maybe [] pure $ Map.lookup wire steiner
    , (gate, cs) <- Map.assocs $ contacts wire
    , (_, pin) <- take 1 cs
    , (tx, ty, _, _) <- take 1 $ portRects $ pinPort pin
    , (x2, y2, _, _) <- maybe [] pure $ Map.lookup gate nodes
    , let target = (x2 + literal tx, y2 + literal ty)
    ]


outerRim steiner nodes = do
  sequence_
    [ liftSMT $ constrain
        $   (i &&& x1 .< x2 ||| o &&& x2 .< x1 ||| bnot i &&& bnot o)
        &&& (i &&& y1 .< y2 ||| o &&& y2 .< y1 ||| bnot i &&& bnot o)
    | (_, (i, o), (x1, y1)) <- toList steiner
    , (x2, y2, _, _) <- toList nodes
    ]


rectangular edges = sequence_ [ liftSMT $ rect path | (_, path) <- edges ]

rect ((x1, y1) : (x2, y2) : xs) = do

  constrain $ x1 .== x2 ||| y1 .== y2
  rect ((x2, y2) : xs)

rect _ = pure ()


freeSteiner (inputs, outputs, _) net = do

  liftSMT $ do
    let i = fromBool $ netIdent net `elem` inputs
        o = fromBool $ netIdent net `elem` outputs
    d <- free_
    x <- free_
    y <- free_

    pure (net, (d, (i, o), (x, y)))


freeNode :: Gate -> LSC (Gate, (SInteger, SInteger, SInteger, SInteger))
freeNode gate = do

  (width, height) <- lookupDimensions gate <$> ask

  liftSMT $ do
    x <- free_
    y <- free_
    w <- free_
    h <- free_

    constrain $ w .== literal width
    constrain $ h .== literal height

    pure (gate, (x, y, w, h))


freeEdge :: LSC [(SInteger, SInteger)]
freeEdge = do

  resolution <- wireResolution <$> ask

  path <- sequence $ replicate resolution $ liftSMT $ (,) <$> free_ <*> free_

  pure path

