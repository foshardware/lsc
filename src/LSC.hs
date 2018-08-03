{-# LANGUAGE GADTs, DataKinds, TupleSections, FlexibleContexts #-}

module LSC where

import Control.Monad.Reader
import Control.Monad.Trans
import Language.SMTLib2
import Language.SMTLib2.Pipe

import LSC.Types


pipeZ3 = createPipe "z3" ["-smt2", "-in"]


stage1 :: Backend b => Netlist -> LSC b String
stage1 (Netlist gates wires) = do
  nodes <- sequence $ lift . newNode <$> gates
  edges <- sequence $ lift . newEdge <$> wires

  distance nodes
  boundedSpace nodes

  connection nodes edges
  
  intersection nodes edges

  lift checkSat
  
  nodesResult <- lift $ mapM ( \ (_, x, y) -> (,) <$> getValue x <*> getValue y ) nodes
  edgesResult <- lift $ mapM ( \ (_, path) -> sequence [(,) <$> getValue x <*> getValue y | (x, y) <- path]) edges
  pure $ show nodesResult ++ "\n" ++ show edgesResult


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
    [ assert

        -- source and target locations
          $ (xStart .==. xSource .+. cint (div sourceFeatX 2) .|. xStart .==. xSource .-. cint (div sourceFeatX 2))
        .&. (yStart .==. ySource .+. cint (div sourceFeatY 2) .|. yStart .==. ySource .-. cint (div sourceFeatY 2))
        .&. (xEnd .==. xEnd.+. cint (div targetFeatX 2) .|. xEnd .==. xTarget .-. cint (div targetFeatX 2))
        .&. (yEnd .==. yEnd.+. cint (div targetFeatY 2) .|. yEnd .==. yTarget .-. cint (div targetFeatY 2))

        -- angles constrained by technology (rectangular)
        .&. and' (rectangular <$> neighbours path)

    | (wire, path) <- edges

    , let (sourceFeatX, sourceFeatY) = featureSize $ source wire
    , let (targetFeatX, targetFeatY) = featureSize $ target wire

    , let (xEnd, yEnd) = last path
    , let (xStart, yStart) = head path

    , (xSource, ySource) <- [ (x, y) | node@(gate, x, y) <- nodes, source wire == gate ]
    , (xTarget, yTarget) <- [ (x, y) | node@(gate, x, y) <- nodes, target wire == gate ]

    ]

rectangular ((x1, y1), (x2, y2)) = x1 .==. x2 .|. y1 .==. y2

neighbours :: [a] -> [(a, a)]
neighbours (x : y : xs) = (x, y) : neighbours (y : xs)
neighbours (x : xs) = neighbours xs
neighbours [] = []


boundedSpace nodes = do
  technology <- ask
  let (xDim, yDim) = dimensions technology
  lift $ sequence_
    [ assert
          $ x .>. cint 0
        .&. y .>. cint 0
        .&. x .<. cint xDim
        .&. y .<. cint yDim
    | (_, x, y) <- nodes
    ]


distance nodes = do
  lift $ sequence_
    [ assert
          $ abs' (x1 .-. x2) .>. cint (1 + div featX1 2 + div featX2 2)
        .|. abs' (y1 .-. y2) .>. cint (1 + div featY1 2 + div featY2 2)
    | node1@(gate1, x1, y1) <- nodes
    , node2@(gate2, x2, y2) <- nodes
    , let (featX1, featY1) = featureSize gate1
    , let (featX2, featY2) = featureSize gate2
    , gate1 /= gate2
    ]


-- upper left corner
newNode gate = (gate, , )
    <$> declareVar int
    <*> declareVar int

newEdge wire = (wire, )
    <$> sequence (replicate 8 newPosition)
    where newPosition = (, ) <$> declareVar int <*> declareVar int

