{-# LANGUAGE GADTs, DataKinds, TupleSections #-}

module Main where

import Language.SMTLib2
import Language.SMTLib2.Pipe


data Netlist = Netlist [Gate] [Wire]

data Wire = Wire Index Index

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

  distances nodes
  boundedSpace technology nodes

  -- intersections edges

  checkSat
  
  mapM ( \ (_, x, y) -> (,) <$> getValue x <*> getValue y ) nodes


boundedSpace technology nodes = within (dimensions technology) `mapM_` nodes
    where within (xd, yd) (_, x, y) = assert $ x .>. cint 0 .&. y .>. cint 0 .&. x .<. cint xd .&. y .<. cint yd

distances nodes = mapM_ overlap [(m, n) | n@(i, _, _) <- nodes , m@(j, _, _) <- nodes, i /= j]
    where overlap ((_, x1, y1), (_, x2, y2)) = assert $ abs' (x1 .-. x2) .>. cint 1 .|. abs' (y1 .-. y2) .>. cint 1

newNode gate = (gate, , ) <$> declareVar int <*> declareVar int

newEdge wire = (wire, , ) <$> declareVar int <*> declareVar int



pipeZ3 = createPipe "z3" ["-smt2", "-in"]

main = do
    let netlist = Netlist [Gate 4 1, Gate 4 2, Gate 4 3] [Wire 1 2, Wire 1 3]
    let tech = Technology (5, 5)
    result <- withBackend pipeZ3 $ stage1 netlist tech
    print result

