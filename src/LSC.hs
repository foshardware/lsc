{-# LANGUAGE GADTs, DataKinds, TupleSections, FlexibleContexts #-}

module LSC where

import Control.Monad.Reader
import Control.Monad.Trans
import Language.SMTLib2
import Language.SMTLib2.Pipe

import LSC.Types


stage1 :: Backend b => Netlist -> LSC b [(Value IntType, Value IntType)]
stage1 (Netlist gates wires) = do
  nodes <- sequence $ lift . newNode <$> gates
  edges <- sequence $ lift . newEdge <$> wires

  distance nodes
  boundedSpace nodes

  connection nodes edges
  
  -- intersections edges

  lift checkSat
  
  lift $ mapM ( \ (_, x, y) -> (,) <$> getValue x <*> getValue y ) nodes


pipeZ3 = createPipe "z3" ["-smt2", "-in"]

connection nodes edges = do
  technology <- ask
  lift $ sequence_
    [ assert $ x1 .>. cint 0
    | ( wire, (x1, y1), (x2, y2), (x3, y3), (x4, y4)
            , (x5, y5), (x6, y6), (x7, y7), (x8, y8)) <- edges
    , (xSource, ySource) <- [ (x, y) | node@(g, x, y) <- nodes, sourceGate wire == gateIndex g ]
    , (xTarget, yTarget) <- [ (x, y) | node@(g, x, y) <- nodes, targetGate wire == gateIndex g ]
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

newEdge wire = (wire, , , , , , , , )
    <$> newPos <*> newPos <*> newPos <*> newPos
    <*> newPos <*> newPos <*> newPos <*> newPos
    where newPos = (, ) <$> declareVar int <*> declareVar int

