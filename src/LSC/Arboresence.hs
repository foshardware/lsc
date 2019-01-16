{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LSC.Arboresence where

import Control.Lens hiding ((.>), inside)
import Control.Monad
import Control.Monad.Trans
import Data.Foldable
import Data.Map (Map, assocs)
import Data.Vector (Vector, (!))
import Data.SBV
import Data.SBV.Control
import Data.Text (unpack)
import System.IO

import LSC.Types


pnr :: NetGraph -> LSC NetGraph
pnr netlist = do

  debug
    [ "start pnr @ module", netlist ^. identifier . to unpack
    , "-", netlist ^. gates . to length . to show, "gates"
    , "-", netlist ^. nets . to length . to show, "nets"
    ]

  liftSMT $ do
    setOption $ ProduceUnsatCores True

  area <- freeRectangle

  nodes <- sequence $ freeGatePolygon <$> (netlist ^. gates)

  rim <- sequence $ freePinPolygon <$> (netlist ^. supercell . pins)

  (ring, grid) <- powerUpAndGround nodes

  edges <- sequence $ arboresence 4 nodes rim <$> (netlist ^. nets)

  placement area ring rim

  disjointGates nodes
  disjointNets edges

  result <- liftSMT $ query $ do
    result <- checkSat
    case result of

      Sat -> do

        pad <- pure <$> getValueRect area
        ps <- sequence $ getValueRect <$> toList ring
        gr <- sequence $ getValueRect <$> grid
        qs <- sequence $  setPinGeometry <$> rim
        ns <- sequence $  setNetGeometry <$> edges
        gs <- sequence $ setGateGeometry <$> nodes

        pure $ netlist
          & supercell .~ AbstractGate pad (gr ++ ps) qs
          & gates     .~ gs
          & nets      .~ ns

      Unsat -> do

        unsat <- getUnsatCore
        liftIO $ sequence_ $ hPutStrLn stderr <$> unsat

        pure netlist

      _ -> do

        reason <- getUnknownReason
        liftIO $ hPutStrLn stderr $ show reason

        pure netlist

  debug ["stop  pnr @ module", netlist ^. identifier . to unpack]

  pure result


alignment nodes rows = do

  liftSMT $ constrain $ sAnd
    [ path ^. l .== x
    | (_, path) <- toList nodes
    | x <- join $ repeat rows
    ]


placement area ring rim = do

  liftSMT $ constrain
    $   view l area .== 0
    .&& view b area .== 0

  outer ring `inside` area

  d <- literal . lambda <$> ask
  sequence_
    [ liftSMT $ constrain
        $   view l path .== view l area
        .&& view l (outer ring) - view l path .> d
    | (pin, path) <- toList rim
    , pin ^. dir == In
    ]
  sequence_
    [ liftSMT $ constrain
        $   view r path .== view r area
        .&& view r path - view r (outer ring) .> d
    | (pin, path) <- toList rim
    , pin ^. dir == Out
    ]
  sequence_
    [ liftSMT $ constrain
        $   view b path .>= view b (inner ring)
        .&&    view t path .<=    view t (inner ring)
    | (_, path) <- toList rim
    ]

  sequence_
    [ disjoint p q
    | ((_, p), (_, q)) <- distinctPairs $ toList rim
    ]

  pure area


disjointGates nodes = do
  sequence_
    [ disjoint p q
    | ((_, p), (_, q)) <- distinctPairs $ toList nodes
    ]


disjointNets edges = do
  sequence_
    [ disjoint p q
    | ((_, as), (_, bs)) <- distinctPairs $ toList edges
    , p <- as
    , q <- bs
    ]


distinctPairs :: [a] -> [(a, a)]
distinctPairs [] = []
distinctPairs (x : xs) = fmap (x, ) xs ++ distinctPairs xs


freeGatePolygon gate = do

  path <- freeRectangle

  dims <- lookupDimensions gate <$> ask
  for_ dims $ \ (w, h) -> liftSMT $ constrain
    $   view r path - view l path .== literal w
    .&& view t path - view b path .== literal h

  pure (gate, path)


freeWirePolygon net = do

  path <- freeRectangle

  (w, h) <- view standardPin <$> ask

  liftSMT $ constrain
      $    width path .== literal w
      .|| height path .== literal h

  pure (net, path)


freePinPolygon pin = do

  path <- freeRectangle

  (w, h) <- view standardPin <$> ask

  liftSMT $ constrain
    $    width path .== literal w
    .&& height path .== literal h

  pure (pin, path)


inside i o = do
  d <- lambda <$> ask
  liftSMT $ constrain
    $   view l i - view l o .> literal d
    .&& view b i - view b o .> literal d
    .&& view r o - view r i .> literal d
    .&& view t o - view t i .> literal d


disjoint p q = do
  d <- lambda <$> ask
  liftSMT $ constrain
    $   view l q - view r p .> literal d
    .|| view l p - view r q .> literal d
    .|| view b p - view t q .> literal d
    .|| view b q - view t p .> literal d


connect p q = do

  liftSMT $ constrain
    $   view r p .== view r q .&& view t p .== view t q
    .|| view l p .== view l q .&& view t p .== view t q
    .|| view r p .== view r q .&& view b p .== view b q
    .|| view l p .== view l q .&& view b p .== view b q

  liftSMT $ softConstrain
    $   view l p .== view l q .&& view b p .== view b q .&& view r p .== view r q
    .|| view l p .== view l q .&& view b p .== view b q .&& view t p .== view t q
    .|| view l p .== view l q .&& view r p .== view r q .&& view t p .== view t q
    .|| view b p .== view b q .&& view r p .== view r q .&& view t p .== view t q


arboresence n nodes rim net = do

  hyperedge <- sequence
    [ do

      wire <- sequence $ replicate n $ snd <$> freeWirePolygon net

      src `connect` head wire
      snk `connect` last wire

      sequence_ [ connect i j | i <- wire | j <- drop 1 wire ]

      pure wire

    | src <- vertices Out
    , snk <- vertices In
    ]

  pure (net, join hyperedge)

  where

    vertices d =
      [ Rect
          (view l src + literal (view l p))
          (view b src + literal (view b p))
          (view l src + literal (view r p))
          (view b src + literal (view t p))
      | (j, assignments) <- assocs $ net ^. contacts
      , source <- assignments
      , source ^. dir == d
      , let (gate, src) = nodes ! view integer j
      , p <- take 1 $ source ^. port . geometry
      ] ++
      [ path
      | (pin, path) <- toList rim
      , pin ^. dir /= d
      , view identifier pin == view identifier net
      ]


powerUpAndGround nodes = do

  ring <- freeRing

  (w, h) <- view standardPin <$> ask

  let xs = [20000, 40000 ..] :: [Integer]
      wire x = ring ^. l & l .~ literal x & r .~ literal (x + w)

  let grid = wire <$> take 2 xs

  d <- literal . lambda <$> ask
  sequence_
    [ inside path $ inner $ ring & r .~ right & l .~ left
    | (_, path) <- toList nodes
    | left  <- join $ repeat $ [ring ^. l] ++ grid
    | right <- join $ repeat $ grid ++ [ring ^. r]
    ]

  liftSMT $ constrain
    $   width (view l ring) .==  width (view r ring)
    .&& width (view r ring) .== literal w

    .&& height (view b ring) .== height (view t ring)
    .&& height (view t ring) .== literal h

  pure (ring, grid)


freeRing = do

  left   <- freeRectangle
  bottom <- freeRectangle
  right  <- freeRectangle
  top    <- freeRectangle

  liftSMT $ constrain
    $    view l left .==  view l bottom .&& view b bottom .==  view b left
    .&&  view l left .==  view l top    .&&    view t top .==  view t left
    .&& view r right .==  view r bottom .&& view b bottom .== view b right
    .&& view r right .==  view r top    .&&    view t top .== view t right

  pure $ Rect left bottom right top


freeRectangle = do

  area <- liftSMT $ Rect <$> free_ <*> free_ <*> free_ <*> free_

  liftSMT $ constrain
    $   width  area .>= 0
    .&& height area .>= 0
    .&& view l area .>= 0
    .&& view b area .>= 0

  pure area


setGateGeometry (gate, xs) = getValueRect xs
  >>= \ path -> pure $ gate & geometry .~ pure path

setPinGeometry (pin, xs) = getValueRect xs
  >>= \ path -> pure $ pin & port .~ Port Metal1 [path]

setNetGeometry (net, edge) = sequence (getValueRect <$> edge)
  >>= \ path -> pure $ net & geometry .~ path

getValueRect path = Rect
  <$> getValue (path ^. l)
  <*> getValue (path ^. b)
  <*> getValue (path ^. r)
  <*> getValue (path ^. t)
