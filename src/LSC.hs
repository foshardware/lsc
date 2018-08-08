{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE GADTs, DataKinds, TupleSections, FlexibleContexts #-}

module LSC where

import Control.Monad.Reader

import qualified Data.Map as Map

import Data.SBV
import Data.SBV.Internals (modelAssocs)

import LSC.Types


type Stage1 = SBool

stage1 :: Netlist -> LSC Stage1
stage1 (Netlist gates wires) = do
  nodes <- Map.fromList <$> sequence (freeNode <$> gates)
  edges <- Map.fromList <$> sequence (freeEdge <$> wires)

  distance nodes
  boundedSpace nodes

  -- connect nodes edges

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
    [ pure ()
    | (wire, resolution, path) <- Map.elems edges
    , let (sourceGate, sourcePin) = source wire
    , let (targetGate, targetPin) = target wire
    ]

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


freeEdge :: Wire -> LSC (Wire, (Integer, SArray Integer Integer))
freeEdge wire = do

  let resolution = 16
  pathArray <- lift $ newArray "wire"

  let path = foldr
        ( \ i p -> writeArray p (literal i :: SInteger) (literal 0 :: SInteger))
        pathArray
        [0 .. resolution - 1]

  pure (wire, (resolution, path))

