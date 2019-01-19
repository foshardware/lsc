{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LSC.Arboresence where

import Control.Lens hiding ((.>), inside)
import Control.Monad
import Control.Monad.Trans
import Data.Default
import Data.Foldable
import Data.Map (Map, assocs)
import Data.Semigroup
import Data.Vector (indexM)
import Data.SBV
import Data.SBV.Control
import Data.Text (unpack)
import System.IO

import LSC.Types


routeSat :: NetGraph -> LSC NetGraph
routeSat netlist = do

  debug
    [ "start routeSat @ module", netlist ^. identifier & unpack
    , "-", netlist ^. gates & length & show, "gates"
    , "-", netlist ^. nets & length & show, "nets"
    ]

  liftSMT $ do
    setOption $ ProduceUnsatCores True

  area <- freeRectangle

  nodes <- sequence $ netlist ^. gates <&> freeGatePolygon

  rim <- sequence $ netlist ^. supercell . pins <&> freePinPolygon

  edges <- sequence $ netlist ^. nets <&> arboresence 2 nodes rim

  (ring, power, ground) <- powerUpAndGround nodes edges

  placement area ring rim

  disjointGates nodes
  disjointNets edges

  result <- liftSMT $ query $ do
    result <- checkSat
    case result of

      Sat -> do

        pad <- pure <$> getLayered area
        ps <- sequence $ getLayered <$> power
        gr <- sequence $ getLayered <$> ground
        qs <- sequence $  setPinGeometry <$> rim
        ns <- sequence $  setNetGeometry <$> edges
        gs <- sequence $ setGateGeometry <$> nodes

        pure $ netlist
          & supercell .~ AbstractGate pad (def & ports .~ ps) (def & ports .~ gr) qs
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

  debug ["stop  routeSat @ module", netlist ^. identifier . to unpack]

  pure result


placement area ring rim = do

  liftSMT $ constrain
      $ area ^. l .== 0
    .&& area ^. b .== 0

  outer ring `inside` area

  d <- literal . lambda <$> ask
  sequence_
    [ liftSMT $ constrain
          $ path ^. l .== area ^. l
        .&& outer ring ^. l - path ^. l .> d
    | (pin, path) <- toList rim
    , pin ^. dir == In
    ]
  sequence_
    [ liftSMT $ constrain
          $ path ^. r .== area ^. r
        .&& path ^. r - outer ring ^. r .> d
    | (pin, path) <- toList rim
    , pin ^. dir == Out
    ]
  sequence_
    [ liftSMT $ constrain
          $ path ^. b .>= inner ring ^. b
        .&& path ^. t .<= inner ring ^. t
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
distinctPairs (x : xs) = fmap (x, ) xs ++ distinctPairs xs
distinctPairs _ = []


freeGatePolygon gate | gate ^. geometry /= mempty = do

  pure $ gate ^. geometry . to head <&> literal
    & setLayers [metal2, metal3]
    & (,) gate

freeGatePolygon gate = do

  path <- freeRectangle
    <&> integrate metal2
    <&> integrate metal3

  dimensions <- lookupDimensions gate <$> ask
  for_ dimensions $ \ (w, h) -> liftSMT $ constrain
      $ view r path - view l path .== literal w
    .&& view t path - view b path .== literal h

  pure (gate, path)


freeWirePolygon = do

  path <- freeRectangle

  (w, h) <- view standardPin <$> ask

  liftSMT $ constrain
      $  width path .== literal w
    .|| height path .== literal h

  pure path


freePinPolygon pin = do

  path <- freeRectangle

  (w, h) <- view standardPin <$> ask

  liftSMT $ constrain
      $  width path .== literal w
    .&& height path .== literal h

  pure (pin, path)


inside i o = do
  d <- lambda <$> ask
  liftSMT $ constrain
      $ view l i - view l o .> literal d
    .&& view b i - view b o .> literal d
    .&& view r o - view r i .> literal d
    .&& view t o - view t i .> literal d


disjoint p q = do
  d <- lambda <$> ask
  liftSMT $ constrain
      $ sAnd [ x ./= y | x <- view z p, y <- view z q ]
    .|| view l q - view r p .> literal d
    .|| view l p - view r q .> literal d
    .|| view b p - view t q .> literal d
    .|| view b q - view t p .> literal d


equivalent p q = do
  liftSMT $ constrain
      $ view r p .== view r q
    .&& view b p .== view b q
    .&& view r p .== view r q
    .&& view t p .== view t q


connect p q = do

  liftSMT $ constrain
      $ view r p .== view r q .&& view t p .== view t q
    .|| view l p .== view l q .&& view t p .== view t q
    .|| view r p .== view r q .&& view b p .== view b q
    .|| view l p .== view l q .&& view b p .== view b q

  liftSMT $ softConstrain
      $ view l p .== view l q .&& view b p .== view b q .&& view r p .== view r q
    .|| view l p .== view l q .&& view b p .== view b q .&& view t p .== view t q
    .|| view l p .== view l q .&& view r p .== view r q .&& view t p .== view t q
    .|| view b p .== view b q .&& view r p .== view r q .&& view t p .== view t q


arboresence n nodes rim net = do

  hyperedge <- sequence
    [ do

      wire <- sequence $ replicate n $ integrate metal1 <$> freeWirePolygon

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
      [ pinComponent src p
      | (j, assignments) <- assocs $ net ^. contacts
      , source <- assignments
      , source ^. dir == d
      , (gate, src) <- indexM nodes $ j ^. integer
      , p <- take 1 $ source ^. ports
      ] ++
      [ path
      | (pin, path) <- toList rim
      , pin ^. dir /= d
      , view identifier pin == view identifier net
      ]


pinComponent p s = p
  & l +~ literal (view l s)
  & b +~ literal (view b s)
  & r .~ view l p + literal (view r s)
  & t .~ view b p + literal (view t s)


powerUpAndGround nodes edges = do

  (w, h) <- view standardPin <$> ask
  rows <- divideArea nodes <$> ask

  grid <- sequence
    [ freeRectangle
      <&> l .~ literal x
      <&> r .~ literal (x + w)
      <&> integrate metal2
      <&> integrate metal3
    | x <- rows
    ]

  sequence_
    [ liftSMT $ constrain
          $ head grid ^. t .== path ^. t
        .&& head grid ^. b .== path ^. b
    | path <- init $ tail $ grid
    ]

  ring <- freeRing

  head grid `equivalent` view l ring
  last grid `equivalent` view r ring

  liftSMT $ constrain
      $ ring ^. l . to width .== literal w
    .&& ring ^. r . to width .== literal w

    .&& ring ^. b . to height .== literal h
    .&& ring ^. t . to height .== literal h

  liftSMT $ constrain
      $ height (outer ring) .< width (outer ring) + width (outer ring)

  (vs, gs) <- unzip <$> sequence
    [ do

      path `inside` inner ring

      sequence_ $ disjoint path <$> grid

      vdd_ <- integrate metal1 <$> freeWirePolygon
      gnd_ <- integrate metal1 <$> freeWirePolygon

      vs <- integrate metal2 <$> freeWirePolygon
      gs <- integrate metal3 <$> freeWirePolygon

      pinComponent path v `connect` vdd_
      pinComponent path g `connect` gnd_

      gnd_ `connect` gs
      vdd_ `connect` vs

      liftSMT $ constrain $ sOr $
        [ p ^. l .== gs ^. l
        | p <- init grid
        ] ++
        [ outer ring ^. t .== gs ^. t
        , outer ring ^. b .== gs ^. b
        ]

      liftSMT $ constrain $ sOr $
        [ p ^. l .== vs ^. l
        | p <- init grid
        ] ++
        [ outer ring ^. t .== vs ^. t
        , outer ring ^. b .== vs ^. b
        ]

      pure ([vs, vdd_], [gs, gnd_])

    | (gate, path) <- toList nodes
    , v <- gate ^. vdd . ports & take 1
    , g <- gate ^. gnd . ports & take 1
    ]

  sequence_
    [ disjoint v p
    | (v, p) <- distinctPairs $ join vs ++ join gs
    ]

  sequence_
    [ disjoint v p
    | v <- join vs ++ join gs
    , (_, p) <- toList nodes
    ]

  sequence_
    [ disjoint v p
    | v <- join vs ++ join gs
    , (_, ps) <- toList edges
    , p <- ps
    ]

  let power  = join vs ++ [ integrate metal2 p | p <- toList ring ++ grid ]
      ground = join gs ++ [ integrate metal3 p | p <- toList ring ++ grid ]

  pure (ring, power, ground)


freeRing = do

  left   <- freeRectangle
  bottom <- freeRectangle
  right  <- freeRectangle
  top    <- freeRectangle

  liftSMT $ constrain
      $ view l  left .== view l bottom .&& view b bottom .== view b  left
    .&& view l  left .== view l    top .&& view t    top .== view t  left
    .&& view r right .== view r bottom .&& view b bottom .== view b right
    .&& view r right .== view r    top .&& view t    top .== view t right

  pure $ Rect left bottom right top


freeRectangle = do

  area <- liftSMT $ Rect <$> free_ <*> free_ <*> free_ <*> free_

  liftSMT $ constrain
      $  width area .>= 0
    .&& height area .>= 0
    .&& area ^. l .>= 0
    .&& area ^. b .>= 0

  pure area


setGateGeometry (gate, rows) = getLayered rows
  >>= \ path -> pure $ gate & geometry .~ pure path

setPinGeometry (pin, rows) = getLayered rows
  >>= \ path -> pure $ pin & ports .~ pure path

setNetGeometry (net, edge) = sequence (getLayered <$> edge)
  >>= \ path -> pure $ net & geometry .~ path

getLayered path = Layered
  <$> getValue (path ^. l)
  <*> getValue (path ^. b)
  <*> getValue (path ^. r)
  <*> getValue (path ^. t)
  <*> mapM getLayerValue (path ^. z)

getLayerValue :: SLayer -> Query Layer
getLayerValue x = toEnum . fromEnum <$> getValue x
