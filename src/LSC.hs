{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE GADTs, DataKinds, TupleSections, FlexibleContexts #-}

module LSC where

import Control.Monad.Reader
import Control.Monad.Trans

import Data.Map as Map


import Data.SBV
import Data.SBV.Tools.CodeGen
import Data.SBV.Internals (Timing(PrintTiming))

import LSC.Types


type Stage1 = Circuit2D

stage1 :: Netlist -> LSC Stage1
stage1 (Netlist gates wires) = do
  nodes <- sequence $ lift . newNode <$> gates
  edges <- sequence $ newEdge <$> wires

  distance nodes
  boundedSpace nodes

  -- connection nodes edges
  
  -- intersection nodes edges

  technology <- ask

  -- minimize component locations
  lift $ forM_ nodes $ \ (_, x, y) ->
      minimize "stage1" x *> minimize "stage1" y

  let nodesResult =
        [ ( maybe 0 id $ unliteral x
          , maybe 0 id $ unliteral y
          , fst $ lookupDimensions technology g
          , snd $ lookupDimensions technology g
          ) | (g, x, y) <- nodes ]

  pure nodesResult


neighbours :: [a] -> [(a, a)]
neighbours (x : y : xs) = (x, y) : neighbours (y : xs)
neighbours (x : xs) = neighbours xs
neighbours [] = []


boundedSpace nodes = do
  technology <- ask
  let (xDim, yDim) = padDimensions technology
  lift $ sequence_
    [ constrain
        $   x .> literal 0
        &&& y .> literal 0
        &&& x .< literal xDim
        &&& y .< literal yDim
    | (_, x, y) <- nodes
    ]


distance nodes = do
  technology <- ask
  lift $ sequence_
    [ constrain
        $   x1 .> x2 &&& x1 - x2 .> literal dimX2
        ||| x2 .> x1 &&& x2 - x1 .> literal dimX1
        ||| y1 .> y2 &&& y1 - y2 .> literal dimY2
        ||| y2 .> y1 &&& y2 - y1 .> literal dimY1
    | node1@(gate1, x1, y1) <- nodes
    , node2@(gate2, x2, y2) <- nodes
    , let (dimX1, dimY1) = lookupDimensions technology gate1
    , let (dimX2, dimY2) = lookupDimensions technology gate2
    , gate1 /= gate2
    ]


-- upper left corner
newNode gate = (gate, , )
    <$> free_
    <*> free_

newEdge :: Wire -> LSC (Wire, [(SInteger, SInteger)])
newEdge wire = (wire, )
    <$> sequence (replicate 8 newPosition)
    where newPosition = (, ) <$> lift free_ <*> lift free_

