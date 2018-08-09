{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE GADTs, DataKinds, TupleSections, FlexibleContexts #-}

module LSC where

import Control.Monad.Reader

import qualified Data.Map as Map

import Data.Maybe

import Data.SBV
import Data.SBV.Control

import LSC.Operator
import LSC.Types


type Stage1 = Circuit2D

stage1 :: Netlist -> LSC Stage1
stage1 (Netlist gates wires) = do

  nodes <- Map.fromList <$> sequence (freeNode <$> gates)
  edges <- Map.fromList <$> sequence (freeEdge <$> wires)

  collision nodes
  boundedSpace nodes

  connect nodes edges

  intersections edges
  -- penetrations nodes edges


  resolution <- wireResolution <$> ask

  lift $ query $ do
    result <- checkSat
    case result of

      Sat -> Circuit2D

        <$> sequence
            [ (, , , ) <$> getValue x <*> getValue y <*> getValue w <*> getValue h
            | (x, y, w, h) <- Map.elems nodes
            ]

        <*> sequence
            [ Path <$> sequence
                [ (, ) <$> getValue (pathX ! i) <*> getValue (pathY ! i)
                | i <- [1 .. resolution]
                ]
            | (pathX, pathY) <- Map.elems edges
            ]

      _   -> pure $ Circuit2D [] []


boundedSpace nodes = do
  (xDim, yDim) <- padDimensions <$> ask
  lift $ sequence_
    [ constrain
        $   x .> literal 0
        &&& y .> literal 0
        &&& x .< literal xDim
        &&& y .< literal yDim
    | (x, y, _, _) <- Map.elems nodes
    ]


collision nodes = do
  lift $ sequence_
    [ constrain
        $   x1 .> x2 &&& x1 - x2 .> w2
        ||| x2 .> x1 &&& x2 - x1 .> w1
        ||| y1 .> y2 &&& y1 - y2 .> h2
        ||| y2 .> y1 &&& y2 - y1 .> h1
    | (i, (x1, y1, w1, h1)) <- zip [1..] $ Map.elems nodes
    ,     (x2, y2, w2, h2)  <- drop i $ Map.elems nodes
    ]


connect nodes edges = do
  resolution <- wireResolution <$> ask
  sequence_
    [ do
      lift $ constrain
        $   x1 + literal sx .== pathX ! 1
        &&& y1 + literal sy .== pathY ! 1
        &&& x2 + literal tx .== pathX ! resolution
        &&& y2 + literal ty .== pathY ! resolution

      rectangular resolution pathX pathY

      shorten wire resolution pathX pathY

    | (wire, (pathX, pathY)) <- Map.assocs edges
    , let (sourceGate, sourcePin) = source wire
    , let (targetGate, targetPin) = target wire
    , (sx, sy, _, _) <- take 1 $ portRects $ pinPort sourcePin
    , (tx, ty, _, _) <- take 1 $ portRects $ pinPort targetPin
    , (x1, y1, _, _) <- maybeToList $ Map.lookup sourceGate nodes
    , (x2, y2, _, _) <- maybeToList $ Map.lookup targetGate nodes
    ]


rectangular r x y
  | r > 1
  = do
    lift $ constrain
      $   x ! r .== x ! (r - 1)
      ||| y ! r .== y ! (r - 1)
    rectangular (r - 1) x y
rectangular _ _ _
  = pure ()


shorten wire resolution pathX pathY = do

  lift $ minimize ("min_p_" ++ show (wireIndex wire))
    $ sum
      [ abs (x1 - x2) + abs (y1 - y2)
      | ((x1, y1), (x2, y2))
              <-  foldr accumulate [] [1 .. resolution]
            `zip` foldr accumulate [] [2 .. resolution]
      ] where accumulate i a = (pathX ! i, pathY ! i) : a


intersections edges = pure ()

intersect wire1 wire2 _ _
  | source wire1 == source wire2
  = pure ()
intersect _ _ line1@((x1, y1), (x2, y2)) line2@((x3, y3), (x4, y4))
  = constrain

  $   point line1 &&& point line2

  ||| point line1 &&& vertical   line2 &&& x1 ./= x3
  ||| point line1 &&& horizontal line2 &&& y1 ./= y3

  ||| vertical line1 &&& vertical   line2 &&& x1 ./= x3
  ||| vertical line1 &&& horizontal line2 &&& y1 ./= y3

  ||| horizontal line1 &&& vertical   line2 &&& x1 ./= x3 -- complex
  ||| horizontal line1 &&& horizontal line2 &&& x1 ./= x3 

  where
    point      ((x1, y1), (x2, y2)) = x1 .== x2 &&& y1 .== y2
    vertical   ((x1, y1), (x2, y2)) = x1 .== x2 &&& y1 ./= y2
    horizontal ((x1, y1), (x2, y2)) = x1 ./= x2 &&& y1 .== y2


freeNode :: Gate -> LSC (Gate, (SInteger, SInteger, SInteger, SInteger))
freeNode gate = do

  technology <- ask

  let suffix = show $ gateIndex gate
      (xDim, yDim) = lookupDimensions technology gate

  lift $ do
    x <- free $ "x_" ++ suffix
    y <- free $ "y_" ++ suffix
    w <- free $ "w_" ++ suffix
    h <- free $ "h_" ++ suffix

    constrain $ w .== literal xDim
    constrain $ h .== literal yDim

    pure (gate, (x, y, w, h))


freeEdge :: Wire -> LSC (Wire, (SArray Integer Integer, SArray Integer Integer))
freeEdge wire = do

  let suffix = show $ wireIndex $ wire

  pathX <- lift $ newArray $ "px_" ++ suffix
  pathY <- lift $ newArray $ "py_" ++ suffix

  pure (wire, (pathX, pathY))

