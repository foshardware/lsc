{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module LSC.Arboresence where

import Control.Applicative
import Control.Arrow
import Control.Exception
import Control.Lens hiding ((.>), inside)
import Control.Monad
import Data.Default
import Data.Foldable
import Data.Map (assocs, lookup, member, singleton)
import Data.Maybe
import Data.Vector (indexM)
import Data.Text (unpack)
import Prelude hiding (lookup)

import LSC.Synthesis
import LSC.Symbolic
import LSC.Types


routeSat :: NetGraph -> LSC NetGraph
routeSat t_ = do

  netlist <- contactGeometry t_

  let abstract = netlist ^. gates <&> view identifier & any (`member` view subcells netlist)

  debug
    [ "start routeSat @ module", netlist ^. identifier & unpack
    , "-", netlist ^. gates & length & show, "gates"
    , "-", netlist ^. nets & length & show, "nets"
    ]

  liftSymbolic $ do
    setOption $ ProduceUnsatCores True
    setLogic QF_IDL

  area <- freeRectangle

  nodes <- sequence $ netlist ^. gates <&> gatePolygon netlist

  rim <- sequence $ netlist ^. supercell . pins <&> pinPolygon

  edges <- sequence $ netlist ^. nets <&> arboresence nodes rim

  (ring, power, ground) <- if abstract
    then powerRingOnly nodes edges
    else powerUpAndGround nodes edges

  placement area ring rim

  disjointGates nodes
  disjointNets edges

  limit <- view halt <$> environment
  result <- liftSymbolic $ query $ do
    result <- timeout limit checkSat
    case result of

      Sat -> do

        pad <- pure <$> getLayered area
        ri <- sequence $ getLayered <$> toList ring
        ps <- sequence $ getLayered <$> power
        gr <- sequence $ getLayered <$> ground
        qs <- sequence $  setPinGeometry <$> rim
        ns <- sequence $  setNetGeometry <$> edges
        gs <- sequence $ setGateGeometry <$> nodes

        pure $ netlist
          & gates     .~ gs
          & nets      .~ ns
          & nets     <>~ singleton "vdd" (Net "vdd" ps mempty)
          & nets     <>~ singleton "gnd" (Net "gnd" gr mempty)
          & supercell .~ AbstractCell pad
            (def & ports .~ fmap (integrate [Metal2]) ri)
            (def & ports .~ fmap (integrate [Metal3]) ri)
            qs

      Unsat -> do

        unsat <- getUnsatCore
        throw $ AssertionFailed $ unlines unsat

      _ -> do

        reason <- getUnknownReason
        throw $ AssertionFailed $ show reason

  debug ["stop  routeSat @ module", netlist ^. identifier & unpack]

  pure result


placement :: SComponent -> SRing -> Pins -> LSC ()
placement area ring rim = do

  liftSymbolic $ constrain
      $ area ^. l .== 0
    .&& area ^. b .== 0

  outer ring `inside` area

  d <- literal . lambda <$> technology
  sequence_
    [ liftSymbolic $ constrain
          $ path ^. l .== area ^. l
        .&& outer ring ^. l - path ^. l .> d
    | (pin, path) <- toList rim
    , pin ^. dir == Just In
    ]
  sequence_
    [ liftSymbolic $ constrain
          $ path ^. r .== area ^. r
        .&& path ^. r - outer ring ^. r .> d
    | (pin, path) <- toList rim
    , pin ^. dir == Just Out
    ]
  sequence_
    [ liftSymbolic $ constrain
          $ path ^. b .>= inner ring ^. b
        .&& path ^. t .<= inner ring ^. t
    | (_, path) <- toList rim
    ]

  sequence_
    [ disjoint p q
    | ((_, p), (_, q)) <- distinctPairs $ toList rim
    ]


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


gatePolygon :: NetGraph -> Gate -> LSC (Gate, SComponent)
gatePolygon _ gate
  | gate ^. geometry /= mempty
  = gate ^. geometry . to head <&> literal
      & integrate [metal2, metal3]
      & (,) gate
      & pure

gatePolygon netlist gate = do

  path <- freeRectangle <&> integrate [metal2, metal3]

  standardDimensions <- lookupDimensions gate <$> technology

  abstractDimensions <- pure $ join $ lookup (gate ^. identifier) (netlist ^. subcells)
    <&> view (supercell . geometry)
    <&> fmap (width &&& height)
    <&> listToMaybe

  let dimensions = abstractDimensions <|> standardDimensions

  for_ dimensions $ \ (w, h) -> liftSymbolic $ constrain
      $ view r path - view l path .== literal w
    .&& view t path - view b path .== literal h

  pure (gate, path)


freeWirePolygon :: LSC SComponent
freeWirePolygon = do

  path <- freeRectangle

  (w, h) <- view standardPin <$> technology

  liftSymbolic $ constrain
      $  width path .== literal w
    .|| height path .== literal h

  pure path


pinPolygon :: Pin -> LSC (Pin, SComponent)
pinPolygon pin = do

  path <- freeRectangle

  (w, h) <- view standardPin <$> technology

  liftSymbolic $ constrain
      $  width path .== literal w
    .&& height path .== literal h

  pure (pin, path)


inside :: SComponent -> SComponent -> LSC ()
inside i o = do
  d <- lambda <$> technology
  liftSymbolic $ constrain
      $ view l i - view l o .> literal d
    .&& view b i - view b o .> literal d
    .&& view r o - view r i .> literal d
    .&& view t o - view t i .> literal d


disjoint :: SComponent -> SComponent -> LSC ()
disjoint p q = do
  d <- lambda <$> technology
  liftSymbolic $ constrain
      $ sAnd [ x ./= y | x <- view z p, y <- view z q ]
    .|| view l q - view r p .> literal d
    .|| view l p - view r q .> literal d
    .|| view b p - view t q .> literal d
    .|| view b q - view t p .> literal d


equivalent :: SComponent -> SComponent -> LSC ()
equivalent p q = do
  liftSymbolic $ constrain
      $ view r p .== view r q
    .&& view b p .== view b q
    .&& view r p .== view r q
    .&& view t p .== view t q


connect :: SComponent -> SComponent -> LSC ()
connect p q = do
  liftSymbolic $ constrain
      $ view r p .== view r q .&& view t p .== view t q
    .|| view l p .== view l q .&& view t p .== view t q
    .|| view r p .== view r q .&& view b p .== view b q
    .|| view l p .== view l q .&& view b p .== view b q


arboresence :: Nodes -> Pins -> Net -> LSC (Net, SPath)
arboresence nodes rim net = do

  n <- succ . view jogs <$> environment

  hyperedge <- sequence
    [ do

      wire <- sequence $ replicate n $ integrate [metal1] <$> freeWirePolygon

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
      , source ^. dir == Just d
      , (_, src) <- indexM nodes j
      , p <- take 1 $ source ^. ports
      ] ++
      [ path
      | (pin, path) <- toList rim
      , pin ^. dir /= Just d
      , view identifier pin == view identifier net
      ]


pinComponent :: SComponent -> Component l Integer -> SComponent
pinComponent p s = p
  & l +~ literal (view l s)
  & b +~ literal (view b s)
  & r .~ view l p + literal (view r s)
  & t .~ view b p + literal (view t s)


powerUpAndGround :: Nodes -> Edges -> LSC (SRing, SPath, SPath)
powerUpAndGround nodes edges = do

  (w, h) <- view standardPin <$> technology
  rows <- divideArea nodes

  grid <- sequence
    [ freeRectangle
      <&> l .~ literal x
      <&> r .~ literal (x + w)
      <&> integrate [metal2, metal3]
    | x <- rows
    ]

  sequence_
    [ liftSymbolic $ constrain
          $ head grid ^. t .== path ^. t
        .&& head grid ^. b .== path ^. b
    | path <- init $ tail $ grid
    ]

  ring <- freeRing

  head grid `equivalent` view l ring
  last grid `equivalent` view r ring

  liftSymbolic $ constrain
      $ ring ^. l . to width .== literal w
    .&& ring ^. r . to width .== literal w

    .&& ring ^. b . to height .== literal h
    .&& ring ^. t . to height .== literal h

  liftSymbolic $ constrain
      $ height (outer ring) .< width (outer ring)

  (vs, gs) <- unzip <$> sequence
    [ do

      path `inside` inner ring

      sequence_ $ disjoint path <$> grid

      vdd_ <- integrate [metal1] <$> freeWirePolygon
      gnd_ <- integrate [metal1] <$> freeWirePolygon

      vs <- integrate [metal2] <$> freeWirePolygon
      gs <- integrate [metal3] <$> freeWirePolygon

      pinComponent path v `connect` vdd_
      pinComponent path g `connect` gnd_

      gnd_ `connect` gs
      vdd_ `connect` vs

      liftSymbolic $ constrain $ sOr $
        [ p ^. l .== gs ^. l
        | p <- init grid
        ] ++
        [ outer ring ^. t .== gs ^. t
        , outer ring ^. b .== gs ^. b
        ]

      liftSymbolic $ constrain $ sOr $
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

  let power  = join vs ++ [ integrate [metal2] p | p <- toList ring ++ grid ]
      ground = join gs ++ [ integrate [metal3] p | p <- toList ring ++ grid ]

  pure (ring, power, ground)


powerRingOnly :: Nodes -> Edges -> LSC (SRing, SPath, SPath)
powerRingOnly nodes edges = do

  let i = head $ fmap (width . snd) (toList nodes) ++ [0]

  (w, h) <- view standardPin <$> technology

  ring <- freeRing

  liftSymbolic $ constrain
      $ ring ^. l . to width .== literal w
    .&& ring ^. r . to width .== literal w

    .&& ring ^. b . to height .== literal h
    .&& ring ^. t . to height .== literal h

  liftSymbolic $ constrain
      $ height (outer ring) .< width (outer ring)
    .&& height (outer ring) .< i + i + i

  (vs, gs) <- unzip <$> sequence
    [ do

      path `inside` inner ring

      vdd_ <- integrate [metal1] <$> freeWirePolygon
      gnd_ <- integrate [metal1] <$> freeWirePolygon

      vs <- integrate [metal2] <$> freeWirePolygon
      gs <- integrate [metal3] <$> freeWirePolygon

      pinComponent path v `connect` vdd_
      pinComponent path g `connect` gnd_

      gnd_ `connect` gs
      vdd_ `connect` vs

      liftSymbolic $ constrain $ sOr
        [ outer ring ^. t .== gs ^. t
        , outer ring ^. b .== gs ^. b
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

  let power  = join vs ++ [ integrate [metal2] p | p <- toList ring ]
      ground = join gs ++ [ integrate [metal3] p | p <- toList ring ]

  pure (ring, power, ground)



freeRing :: LSC SRing
freeRing = do

  p <- Rect
    <$> freeRectangle
    <*> freeRectangle
    <*> freeRectangle
    <*> freeRectangle

  liftSymbolic $ constrain
      $ p ^. l . l .== p ^. b . l .&& p ^. b . b .== p ^. l . b
    .&& p ^. l . l .== p ^. t . l .&& p ^. t . t .== p ^. l . t
    .&& p ^. r . r .== p ^. b . r .&& p ^. b . b .== p ^. r . b
    .&& p ^. r . r .== p ^. t . r .&& p ^. t . t .== p ^. r . t

  pure p


freeRectangle :: LSC SComponent
freeRectangle = do

  area <- liftSymbolic $ Rect <$> free_ <*> free_ <*> free_ <*> free_

  liftSymbolic $ constrain
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
