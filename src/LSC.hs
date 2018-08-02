{-# LANGUAGE GADTs, DataKinds, TupleSections, FlexibleContexts #-}

module LSC where

import Control.Monad.Reader
import Control.Monad.Trans
import Language.SMTLib2
import Language.SMTLib2.Pipe

import LSC.Types


stage1 :: Backend b => Netlist -> LSC b String
stage1 (Netlist gates wires) = do
  nodes <- sequence $ lift . newNode <$> gates
  edges <- sequence $ lift . newEdge <$> wires

  distance nodes
  boundedSpace nodes

  connection nodes edges
  
  -- intersections edges

  lift checkSat
  
  nodesResult <- lift $ mapM ( \ (_, x, y) -> (,) <$> getValue x <*> getValue y ) nodes
  edgesResult <- lift $ mapM ( \ (_, (x1, y1), (x2, y2), (x3, y3), (x4, y4)) -> (,,,,,,,)
            <$> getValue x1 <*> getValue y1
            <*> getValue x2 <*> getValue y2
            <*> getValue x3 <*> getValue y3
            <*> getValue x4 <*> getValue y4) edges
  pure $ show nodesResult ++ "\n" ++ show edgesResult


pipeZ3 = createPipe "z3" ["-smt2", "-in"]

connection nodes edges = do
  technology <- ask
  lift $ sequence_
    [ assert
        -- source and target locations
          $ x1 .==. xSource
        .&. y1 .==. ySource
        .&. x4 .==. xTarget
        .&. y4 .==. yTarget
        -- angles constrained by technology (rectangular)
        .&. (x1 .==. x2 .|. y1 .==. y2)
        .&. (x2 .==. x3 .|. y2 .==. y3)
        .&. (x3 .==. x4 .|. y3 .==. y4)

    | (wire, (x1, y1), (x2, y2), (x3, y3), (x4, y4)) <- edges
    , (xSource, ySource) <- [ (x, y) | node@(gate, x, y) <- nodes, source wire == gate ]
    , (xTarget, yTarget) <- [ (x, y) | node@(gate, x, y) <- nodes, target wire == gate ]
    ]

boundedSpace nodes = do
  technology <- ask
  lift $ sequence_
    [ assert
          $ x .>. cint 0
        .&. y .>. cint 0
        .&. x .<. cint xDim
        .&. y .<. cint yDim
    | (_, x, y) <- nodes
    , let (xDim, yDim) = dimensions technology
    ]

distance nodes = do
  lift $ sequence_
    [ assert
          $ abs' (x1 .-. x2) .>. cint 1
        .|. abs' (y1 .-. y2) .>. cint 1
    | node1@(gate1, x1, y1) <- nodes
    , node2@(gate2, x2, y2) <- nodes
    , gate1 /= gate2
    ]

newNode gate = (gate, , )
    <$> declareVar int
    <*> declareVar int

newEdge wire = (wire, , , , )
    <$> newPos <*> newPos <*> newPos <*> newPos
    where newPos = (, ) <$> declareVar int <*> declareVar int

