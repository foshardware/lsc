{-# LANGUAGE GADTs, DataKinds, TupleSections #-}

module Main where

import Language.SMTLib2
import Language.SMTLib2.Pipe


data Netlist = Netlist [Gate] [Wire]

data Wire = Wire 
  { sourceGate :: Index
  , targetGate :: Index
  , wireIndex :: Index
  }

type Index = Int

data Gate = Gate
  { featureSize :: Integer
  , gateIndex :: Index
  }
  deriving Eq


  
data Technology = Technology
  { dimensions :: (Integer, Integer)
  }


stage1 :: Backend b => Netlist -> Technology -> SMT b [(Value IntType, Value IntType)]
stage1 (Netlist gates wires) technology = do
  nodes <- sequence $ newNode <$> gates
  edges <- sequence $ newEdge <$> wires

  distance nodes
  boundedSpace technology nodes

  connection technology nodes edges
  
  -- intersections edges

  checkSat
  
  mapM ( \ (_, x, y) -> (,) <$> getValue x <*> getValue y ) nodes


connection technology nodes edges = sequence_
    [ assert $ x1 .>. cint 0
    | ( wire, (x1, y1), (x2, y2), (x3, y3), (x4, y4)
            , (x5, y5), (x6, y6), (x7, y7), (x8, y8)) <- edges
    , (xSource, ySource) <- [ (x, y) | node@(g, x, y) <- nodes, sourceGate wire == gateIndex g ]
    , (xTarget, yTarget) <- [ (x, y) | node@(g, x, y) <- nodes, targetGate wire == gateIndex g ]
    ]

boundedSpace technology nodes = sequence_
    [ assert
          $ x .>. cint 0
        .&. y .>. cint 0
        .&. x .<. cint xDim
        .&. y .<. cint yDim
    | (_, x, y) <- nodes
    , let (xDim, yDim) = dimensions technology
    ]

distance nodes = sequence_
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




pipeZ3 = createPipe "z3" ["-smt2", "-in"]

main = do
    let netlist = Netlist [Gate 4 1, Gate 4 2, Gate 4 3] [Wire 1 2 1, Wire 1 3 2]
    let tech = Technology (5, 5)
    result <- withBackend pipeZ3 $ stage1 netlist tech
    print result

