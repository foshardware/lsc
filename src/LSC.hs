{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE GADTs, DataKinds, TupleSections, FlexibleContexts #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LSC where

import Control.Monad
import Control.Monad.Trans
import Data.Foldable
import qualified Data.Map as Map
import Data.SBV
import Data.SBV.Control
import System.IO

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
  . exline (repeat 20)


pnr :: NetGraph -> LSC Circuit2D
pnr (NetGraph _ pins _ gates wires) = do

  liftSMT $ do
    setOption $ ProduceUnsatCores True

  nodes <- Map.fromList <$> sequence (freeNode <$> toList gates)
  steiner <- Map.fromList <$> sequence (freeSteiner pins <$> toList wires)

  edges <- connect nodes wires steiner

  collision nodes
  boundedSpace nodes

  rectilinear edges

  outerRim steiner nodes

  -- intersections edges

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
                [ (, ) <$> getValue x <*> getValue y | (x, y) <- path ]
            | (net, path) <- edges
            ]

      Unsat -> do

        unsat <- getUnsatCore
        liftIO $ sequence_ $ hPutStrLn stderr <$> unsat

        pure $ Circuit2D [] []

      _ -> do

        reason <- getUnknownReason
        liftIO $ hPutStrLn stderr $ show reason

        pure $ Circuit2D [] []


boundedSpace nodes = do
  (width, height) <- padDimensions <$> ask
  sequence_
    [ liftSMT $ constrain
        $   x .> literal 0
        &&& y .> literal 0
        &&& x .< literal width
        &&& y .< literal height
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


intersections ((net1, path1) : (net2, path2) : edges) = do

  sequence_
    [ overlap a b
    | a <- path1 `zip` drop 1 path1
    , b <- path2 `zip` drop 1 path2
    , net1 /= net2
    ]

  intersections ((net2, path2) : edges)

intersections _ = pure ()


overlap ((sx1, sy1), (sx2, sy2)) ((tx1, ty1), (tx2, ty2)) = do

  liftSMT $ constrain
    $   (sx1 - sx2) .== literal 0 &&& (tx1 - tx2) .== literal 0
    ||| (sy1 - sy2) .== literal 0 &&& (ty1 - ty2) .== literal 0

    ||| (tx1 - tx2) .== literal 0 &&& tx1 .< sx1 &&& tx1 .> sx2
    ||| (ty1 - ty2) .== literal 0 &&& ty1 .< sy1 &&& ty1 .> sy2

    ||| (sx1 - sx2) .== literal 0 &&& sx1 .< tx1 &&& sx1 .> tx2
    ||| (sy1 - sy2) .== literal 0 &&& sy1 .< ty1 &&& sy1 .> ty2


connect nodes wires steiner = do
  sequence
    [ do
      path <- freeEdge

      liftSMT $ constrain
        $   manhattan source target .== distance
        &&& source .== head path
        &&& target .== last path

      pure (wire, path)

    | wire <- toList wires
    , (distance, _, source) <- maybe [] pure $ Map.lookup wire steiner
    , (gate, cs) <- Map.assocs $ contacts wire
    , (_, pin) <- take 1 cs
    , (tx, ty, _, _) <- take 1 $ portRects $ pinPort pin
    , (x2, y2, _, _) <- maybe [] pure $ Map.lookup gate nodes
    , let target = (x2 + literal tx, y2 + literal ty)
    ]


outerRim steiner nodes = do
  sequence_
    [ do
      liftSMT $ constrain $ i ==> x1 .< x2 &&& y1 .< y2
      liftSMT $ constrain $ o ==> x2 + w .< x1 &&& y2 + h .< y1
    | (_, (i, o), (x1, y1)) <- toList steiner
    , (x2, y2, w, h) <- toList nodes
    ]


manhattan :: (SInteger, SInteger) -> (SInteger, SInteger) -> SInteger
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)


rectilinear edges = do
  sequence_
    [ do
      v <- liftSMT free_
      h <- liftSMT free_

      rect v h path

    | (_, path) <- edges
    ]

rect v h ((x1, y1) : (x2, y2) : xs) = do

  liftSMT $ constrain
    $   x1 .== x2 &&& y1 .== y2
    ||| x1 .== x2 &&& v .== (y1 - y2 .< literal 0)
    ||| y1 .== y2 &&& h .== (x1 - x2 .< literal 0)
  rect v h ((x2, y2) : xs)

rect _ _ _ = pure ()


freeSteiner (inputs, outputs, _) net = do

  liftSMT $ do
    let i = fromBool $ netIdent net `elem` inputs
        o = fromBool $ netIdent net `elem` outputs
    d <- free_
    x <- free_
    y <- free_

    pure (net, (d, (i, o), (x, y)))


freeNode gate = do

  (width, height) <- lookupDimensions gate <$> ask

  liftSMT $ do
    x <- free_
    y <- free_
    w <- free_
    h <- free_

    constrain $ w .== literal width
    constrain $ h .== literal height

    pure (gate, (x, y, w, h))


freeEdge = do

  resolution <- wireResolution <$> ask

  path <- sequence $ replicate resolution $ liftSMT $ (,) <$> free_ <*> free_

  pure path

