{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE GADTs, DataKinds, TupleSections, FlexibleContexts #-}

module LSC where

import Control.Monad.Reader
import Control.Monad.Trans

import Data.Map as Map

import Language.SMTLib2
import Language.SMTLib2.Pipe

import LSC.Types


pipeZ3 = createPipe "z3" ["-smt2", "-in"]


type Stage1 = Circuit2D

stage1 :: Backend b => Netlist -> LSC b Stage1
stage1 (Netlist gates wires) = do
  nodes <- sequence $ lift . newNode <$> gates
  edges <- sequence $ lift . newEdge <$> wires

  distance nodes
  boundedSpace nodes

  -- connection nodes edges
  
  -- intersection nodes edges

  lift checkSat
  
  technology <- ask
  nodesResult <- lift $ forM nodes $ \ (g, x, y) -> do
    (, , , )
      <$> (fromIntegral <$> getValue x)
      <*> (fromIntegral <$> getValue y)
      <*> (pure $ fst $ lookupDimensions technology g)
      <*> (pure $ snd $ lookupDimensions technology g)
  edgesResult <- lift $ mapM ( \ (_, path) -> sequence [(,) <$> getValue x <*> getValue y | (x, y) <- path]) edges

  pure nodesResult


intersection nodes edges = do
  technology <- ask
  let cut = 1 + wireWidth technology
  lift $ sequence_
    [ assert

          -- forbid intersections of wires
          $ (abs' (x1 .-. x2) .>=. cint cut .|. abs' (y1 .-. y2) .>=. cint cut)

    | edge1@(wire1, path1) <- edges
    , edge2@(wire2, path2) <- edges
    , wire1 /= wire2
    , (x1, y1) <- path1
    , (x2, y2) <- path2
    ]

connection nodes edges = do
  technology <- ask
  lift $ sequence_
    [
    ]

rectangular ((x1, y1), (x2, y2)) = x1 .==. x2 .|. y1 .==. y2

neighbours :: [a] -> [(a, a)]
neighbours (x : y : xs) = (x, y) : neighbours (y : xs)
neighbours (x : xs) = neighbours xs
neighbours [] = []


boundedSpace nodes = do
  technology <- ask
  let (xDim, yDim) = padDimensions technology
  lift $ sequence_
    [ assert
        $   x .>. cint 0
        .&. y .>. cint 0
        .&. x .<. cint xDim
        .&. y .<. cint yDim
    | (_, x, y) <- nodes
    ]


distance nodes = do
  technology <- ask
  lift $ sequence_
    [ assert
        $   x1 .>. x2 .&. x1 .-. x2 .>. cint dimX2
        .|. x2 .>. x1 .&. x2 .-. x1 .>. cint dimX1
        .|. y1 .>. y2 .&. y1 .-. y2 .>. cint dimY2
        .|. y2 .>. y1 .&. y2 .-. y1 .>. cint dimY1
    | node1@(gate1, x1, y1) <- nodes
    , node2@(gate2, x2, y2) <- nodes
    , let (dimX1, dimY1) = lookupDimensions technology gate1
    , let (dimX2, dimY2) = lookupDimensions technology gate2
    , gate1 /= gate2
    ]


-- upper left corner
newNode gate = (gate, , )
    <$> declareVar int
    <*> declareVar int

newEdge wire = (wire, )
    <$> sequence (replicate 8 newPosition)
    where newPosition = (, ) <$> declareVar int <*> declareVar int

