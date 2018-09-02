{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE GADTs, DataKinds, TupleSections, FlexibleContexts #-}
{-# LANGUAGE ParallelListComp #-}

module LSC where

import Control.Monad.Trans
import Data.Foldable
import qualified Data.Map as Map
import qualified Data.Vector as Vector

import Data.SBV
import Data.SBV.Control

import LSC.Operator
import LSC.NetGraph
import LSC.Types
import LSC.Exlining


type Stage1 = Circuit2D

stage1 :: Int -> NetGraph -> LSC Stage1
stage1 j
  = fmap head
  . concLSC
  . take j
  . fmap pnr
  . getLeaves
  . exlineRounds (replicate 7 4)


pnr :: NetGraph -> LSC Circuit2D
pnr (NetGraph _ _ _ gates wires) = do

  nodes <- Map.fromList <$> sequence (freeNode <$> toList gates)
  edges <- Map.fromList <$> sequence (freeEdge <$> toList wires)

  collision nodes
  boundedSpace nodes

  connect nodes edges

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
    | (i, (x1, y1, w1, h1)) <- zip [ 1 .. ] $ Map.elems nodes
    ,     (x2, y2, w2, h2)  <- drop i $ Map.elems nodes
    ]


connect nodes edges = do
  resolution <- wireResolution <$> ask
  sequence_
    [ do
      liftSMT $ constrain
        $   x1 + literal sx .== pathX ! 1
        &&& y1 + literal sy .== pathY ! 1
        &&& x2 + literal tx .== pathX ! resolution
        &&& y2 + literal ty .== pathY ! resolution

      rectangular resolution pathX pathY

      shorten wire resolution pathX pathY

    | (wire, (pathX, pathY)) <- Map.assocs edges
    , let (sourceGate, cs) = head $ Map.assocs $ contacts wire
    , let (targetGate, ds) = last $ Map.assocs $ contacts wire
    , ((_, sourcePin), (_, targetPin)) <- cs `zip` ds
    , (sx, sy, _, _) <- take 1 $ portRects $ pinPort sourcePin
    , (tx, ty, _, _) <- take 1 $ portRects $ pinPort targetPin
    , (x1, y1, _, _) <- maybe [] pure $ Map.lookup sourceGate nodes
    , (x2, y2, _, _) <- maybe [] pure $ Map.lookup targetGate nodes
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

