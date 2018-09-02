{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE GADTs, DataKinds, TupleSections, FlexibleContexts #-}

module LSC where

import Data.Foldable
import qualified Data.Map as Map
import qualified Data.Vector as Vector

import Data.SBV
import Data.SBV.Control

import LSC.Operator
import LSC.NetGraph
import LSC.Types


type Stage1 = Circuit2D

stage1 :: Int -> NetGraph -> LSC Stage1
stage1 j netlist
  = fmap head
  $ concLSC
  $ fmap pnr
  $ take j
  $ getLeaves netlist


pnr :: NetGraph -> LSC Circuit2D
pnr (NetGraph _ _ _ gates wires) = do

  nodes <- Map.fromList <$> sequence (freeNode <$> toList gates)
  edges <- Map.fromList <$> sequence (freeEdge <$> toList wires)

  collision nodes
  boundedSpace nodes

  connect edges

  -- intersections edges

  resolution <- wireResolution <$> ask

  liftSMT $ query $ do
    result <- checkSat
    case result of

      Sat -> Circuit2D

        <$> sequence
            [ fmap (gate, ) $ (,,,) <$> getValue x <*> getValue y <*> getValue w <*> getValue h
            | (gate, (x, y, w, h)) <- Map.assocs nodes
            ]

        <*> sequence
            [ fmap (net, ) $ Path <$> sequence
                [ (, ) <$> getValue (pathX ! i) <*> getValue (pathY ! i)
                | i <- [1 .. resolution]
                ]
            | (net, (pathX, pathY)) <- Map.assocs edges
            ]

      _   -> pure $ Circuit2D [] []


boundedSpace nodes = do
  (xDim, yDim) <- padDimensions <$> ask
  sequence_
    [ liftSMT $ constrain
        $   x .> literal 0
        &&& y .> literal 0
        &&& x .< literal xDim
        &&& y .< literal yDim
    | (x, y, _, _) <- Map.elems nodes
    ]


collision nodes = do
  sequence_
    [ liftSMT $ constrain
        $   x1 .> x2 &&& x1 - x2 .> w2
        ||| x2 .> x1 &&& x2 - x1 .> w1
        ||| y1 .> y2 &&& y1 - y2 .> h2
        ||| y2 .> y1 &&& y2 - y1 .> h1
    | (i, (x1, y1, w1, h1)) <- zip [1..] $ Map.elems nodes
    ,     (x2, y2, w2, h2)  <- drop i $ Map.elems nodes
    ]


connect edges = do
  resolution <- wireResolution <$> ask
  sequence_
    [ do
      pure ()
    ]


rectangular r x y
  | r > 1
  = do
    liftSMT $ constrain
      $   x ! r .== x ! (r - 1)
      ||| y ! r .== y ! (r - 1)
    rectangular (r - 1) x y
rectangular _ _ _
  = pure ()


shorten net resolution pathX pathY = do

  let goal = "min_p_" ++ show (netIndex net)

  liftSMT $ minimize goal
    $ sum
      [ abs (x1 - x2) + abs (y1 - y2)
      | ((x1, y1), (x2, y2))
              <-  foldr accumulate [] [1 .. resolution]
            `zip` foldr accumulate [] [2 .. resolution]
      ] where accumulate i a = (pathX ! i, pathY ! i) : a


freeNode :: Gate -> LSC (Gate, (SInteger, SInteger, SInteger, SInteger))
freeNode gate = do

  technology <- ask

  let suffix = show $ gateIndex gate
      (xDim, yDim) = lookupDimensions technology gate

  liftSMT $ do
    x <- free $ "x_" ++ suffix
    y <- free $ "y_" ++ suffix
    w <- free $ "w_" ++ suffix
    h <- free $ "h_" ++ suffix

    constrain $ w .== literal xDim
    constrain $ h .== literal yDim

    pure (gate, (x, y, w, h))


freeEdge :: Net -> LSC (Net, (SArray Integer Integer, SArray Integer Integer))
freeEdge net = do

  let suffix = show $ netIndex net

  pathX <- liftSMT $ newArray $ "px_" ++ suffix
  pathY <- liftSMT $ newArray $ "py_" ++ suffix

  pure (net, (pathX, pathY))

