{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE GADTs, DataKinds, TupleSections, FlexibleContexts #-}

module LSC where

import Control.Monad.Reader

import qualified Data.Map as Map

import Data.Maybe

import Data.SBV
import Data.SBV.Internals (modelAssocs)

import LSC.Operator
import LSC.Types


type Stage1 = SBool

stage1 :: Netlist -> LSC Stage1
stage1 (Netlist gates wires) = do
  nodes <- Map.fromList <$> sequence (freeNode <$> gates)
  edges <- Map.fromList <$> sequence (freeEdge <$> wires)

  distance nodes
  boundedSpace nodes

  connect nodes edges

  lift $ sBool "satisfiable"


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


distance nodes = do
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
  lift $ sequence_
    [ do
      constrain
        $   x1 + literal sx .== pathX ! 1
        &&& y1 + literal sy .== pathY ! 1
        &&& x2 + literal tx .== pathX ! r
        &&& y2 + literal ty .== pathY ! r

      rectangular r pathX pathY

    | (wire, (r, pathX, pathY)) <- Map.assocs edges
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
    constrain
      $   x ! r .== x ! (r - 1)
      ||| y ! r .== y ! (r - 1)
    rectangular (r - 1) x y
rectangular _ _ _
  = pure ()


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


lexNodes :: SatResult -> [Rectangle]
lexNodes (SatResult (Satisfiable _ prop)) = go $ modelAssocs prop

  where

    go xs@(('x' : _ : _, _) : _) = path as ++ go bs
          where (as, bs) = splitAt 4 xs
    go _ = []

    path ((_, x) : (_, y) : (_, w) : (_, h) : _) = [(fromCW x, fromCW y, fromCW w, fromCW h)]
    path _ = []

lexNodes _ = []


freeEdge :: Wire -> LSC (Wire, (Integer, SArray Integer Integer, SArray Integer Integer))
freeEdge wire = do

  let resolution = 16
  pathX <- lift $ newArray_
  pathY <- lift $ newArray_

  pure (wire, (resolution, pathX, pathY))

