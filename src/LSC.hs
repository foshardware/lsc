{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE GADTs, DataKinds, TupleSections, FlexibleContexts #-}

module LSC where

import Control.Monad.Reader
import Control.Monad.Trans

import Data.Map as Map

import Data.SBV
import Data.SBV.Tools.CodeGen
import Data.SBV.Internals (Timing(PrintTiming), modelAssocs)

import LSC.Types


type Stage1 = SBool

stage1 :: Netlist -> LSC Stage1
stage1 (Netlist gates wires) = do
  nodes <- sequence $ newNode <$> gates
  -- edges <- sequence $ newEdge <$> wires

  distance nodes
  boundedSpace nodes

  lift $ sBool "satisfiable"


boundedSpace nodes = do
  (xDim, yDim) <- padDimensions <$> ask
  lift $ sequence_
    [ constrain
        $   x .> literal 0
        &&& y .> literal 0
        &&& x .< literal xDim
        &&& y .< literal yDim
    | (_, x, y, _, _) <- nodes
    ]


distance nodes = do
  lift $ sequence_
    [ constrain
        $   x1 .> x2 &&& x1 - x2 .> w2
        ||| x2 .> x1 &&& x2 - x1 .> w1
        ||| y1 .> y2 &&& y1 - y2 .> h2
        ||| y2 .> y1 &&& y2 - y1 .> h1
    | node1@(gate1, x1, y1, w1, h1) <- nodes
    , node2@(gate2, x2, y2, w2, h2) <- nodes
    , gate1 /= gate2
    ]


-- upper left corner
newNode :: Gate -> LSC (Gate, SInteger, SInteger, SInteger, SInteger)
newNode g = do

  technology <- ask

  let suffix = show $ gateIndex g
      (xDim, yDim) = lookupDimensions technology g

  lift $ do
    x <- free $ "x_" ++ suffix
    y <- free $ "y_" ++ suffix
    w <- free $ "w_" ++ suffix
    h <- free $ "h_" ++ suffix

    constrain $ w .== literal xDim
    constrain $ h .== literal yDim

    pure (g, x, y, w, h)


lexNodes :: SatResult -> [Rectangle]
lexNodes (SatResult (Satisfiable _ prop)) = lex $ modelAssocs prop

  where

    lex xs@(('x' : _ : index, _) : _) = path as ++ lex bs
          where (as, bs) = splitAt 4 xs
    lex _ = []

    path ((_, x) : (_, y) : (_, w) : (_, h) : _) = [(fromCW x, fromCW y, fromCW w, fromCW h)]
    path _ = []