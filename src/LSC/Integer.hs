{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ParallelListComp #-}

module LSC.Integer where

import Control.Arrow
import Control.Applicative
import Control.Monad

import Control.Lens hiding (inside)
import Data.Default
import Data.Foldable
import Data.Maybe
import Data.Text (unpack)
import Data.Map (lookup, assocs, insert)
import Data.Vector (indexM)

import Prelude hiding (lookup)

import Control.MILP.Types

import LSC.Synthesis
import LSC.Types


routeInteger :: NetGraph -> LSC NetGraph
routeInteger top = do

  netlist <- contactGeometry top

  debug
    [ "start routeInteger @ module", netlist ^. identifier & unpack
    , "-", netlist ^. gates & length & show, "gates"
    , "-", netlist ^. nets & length & show, "nets"
    ]

  area <- freeRectangle

  nodes <- sequence $ netlist ^. gates <&> gatePolygon netlist

  rim <- sequence $ netlist ^. supercell . pins <&> pinPolygon

  edges <- sequence $ netlist ^. nets <&> arboresence nodes rim

  (ring, power, ground) <- powerUpAndGround nodes edges

  placement area ring rim

  disjointGates nodes
  disjointNets edges

  result <- satisfyInteger

  debug ["stop  routeInteger @ module", netlist ^. identifier & unpack]

  maybe (pure netlist) pure $ do

    pad <- getLayered result area
    ri <- sequence $ getLayered result <$> toList ring
    ps <- sequence $ getLayered result <$> power
    gr <- sequence $ getLayered result <$> ground
    qs <- sequence $ setPinGeometry result <$> rim
    ns <- sequence $ setNetGeometry result <$> edges
    gs <- sequence $ setGateGeometry result <$> nodes
    
    pure $ netlist &~ do
      gates .= gs
      nets  .= ns
      nets  %= insert "vdd" (Net "vdd" ps mempty)
      nets  %= insert "gnd" (Net "gnd" gr mempty)
      supercell .= AbstractCell [pad]
        (def & ports .~ fmap (integrate [Metal2]) ri)
        (def & ports .~ fmap (integrate [Metal3]) ri)
        qs



powerUpAndGround :: INodes -> IEdges -> LSC (IRing, IPath, IPath)
powerUpAndGround nodes edges = do

  (w, h) <- view standardPin <$> technology
  rows <- divideArea nodes

  grid <- sequence
    [ do
      p <- liftInteger general
      liftInteger $ do
        p >=^ x
        p <=^ x + 8000
      freeRectangle
        <&> l .~ p
        <&> r .~ (p + literal w)
        <&> integrate [Metal2, Metal3]
    | x <- literal <$> rows
    ]

  sequence_
    [ liftInteger $ do
        head grid ^. t =^ path ^. t
        head grid ^. b =^ path ^. b
    | path <- init $ tail $ grid
    ]

  ring <- freeRing

  liftInteger $ do

    head grid `equivalent` view l ring
    last grid `equivalent` view r ring

    ring ^. l . to width =^ literal w
    ring ^. r . to width =^ literal w

    ring ^. b . to height =^ literal h
    ring ^. t . to height =^ literal h

  liftInteger $ height (outer ring) <=^ width (outer ring)

  (vs, gs) <- unzip <$> sequence
    [ do

      path `inside` inner ring

      sequence_ $ disjoint path <$> grid

      vdd_ <- integrate [Metal1] <$> freeWirePolygon
      gnd_ <- integrate [Metal1] <$> freeWirePolygon

      v <- integrate [Metal2] <$> freeWirePolygon
      g <- integrate [Metal3] <$> freeWirePolygon

      liftInteger $ do
        vs `anyConnect` vdd_
        gs `anyConnect` gnd_
        gnd_ `connect` g
        vdd_ `connect` v

      cell <- lookup (gate ^. identifier) . view stdCells <$> technology

      let signals = foldr accum [] $ maybe mempty (view pins) cell
          accum x a = x ^. ports <&> pinComponent path & mappend a

      sequence_ [ disjoint sig gnd_ *> disjoint sig vdd_ | sig <- signals ]


      liftInteger $ foldr (<|>) empty $
        [ p ^. l =^ g ^. l
        | p <- init grid
        ] ++
        [ outer ring ^. t =^ g ^. t
        , outer ring ^. b =^ g ^. b
        ]

      liftInteger $ foldr (<|>) empty $
        [ p ^. l =^ v ^. l
        | p <- init grid
        ] ++
        [ outer ring ^. t =^ v ^. t
        , outer ring ^. b =^ v ^. b
        ]

      pure ([v, vdd_], [g, gnd_])

    | (gate, path) <- toList nodes
    , let vs = gate ^. vdd . ports <&> pinComponent path
    , let gs = gate ^. gnd . ports <&> pinComponent path
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

  let power  = join vs ++ [ integrate [Metal2] p | p <- toList ring ++ grid ]
      ground = join gs ++ [ integrate [Metal3] p | p <- toList ring ++ grid ]

  pure (ring, power, ground)


arboresence :: INodes -> IPins -> Net -> LSC (Net, IPath)
arboresence nodes rim net = do

  n <- succ . view jogs <$> environment

  hyperedge <- sequence
    [ do

      wire <- sequence $ replicate n $ integrate [Metal1] <$> freeWirePolygon

      liftInteger $ do
        -- head src `connect` head wire
        -- head snk `connect` last wire
        src `anyConnect` head wire
        snk `anyConnect` last wire
        

      sequence_ [ liftInteger $ connect i j | i <- wire | j <- drop 1 wire ]

      pure wire

    | src <- vertices Out
    , snk <- vertices In
    ]

  pure (net, join hyperedge)

  where

    vertices d =
      [ source ^. ports <&> pinComponent src
      | (j, assignments) <- assocs $ net ^. contacts
      , source <- assignments
      , source ^. dir == Just d
      , (_, src) <- indexM nodes j
      ] ++
      [ pure path
      | (pin, path) <- toList rim
      , pin ^. dir /= Just d
      , view identifier pin == view identifier net
      ]



equivalent :: IComponent -> IComponent -> LP ()
equivalent p q = do
  view r p =^ view r q
  view b p =^ view b q
  view r p =^ view r q
  view t p =^ view t q


connect :: IComponent -> IComponent -> LP ()
connect p q
      = (view r p =^ view r q >> view t p =^ view t q)
    <|> (view l p =^ view l q >> view t p =^ view t q)
    <|> (view r p =^ view r q >> view b p =^ view b q)
    <|> (view l p =^ view l q >> view b p =^ view b q)


anyConnect :: Foldable f => f IComponent -> IComponent -> LP ()
anyConnect qs p = foldr (\ q a -> a <|> connect p q) mzero qs


pinComponent :: IComponent -> Component l Integer -> IComponent
pinComponent p s = p
  & l +~ literal (view l s)
  & b +~ literal (view b s)
  & r .~ view l p + literal (view r s)
  & t .~ view b p + literal (view t s)
  & integrate [Metal1]



freeWirePolygon :: LSC IComponent
freeWirePolygon = do

  path <- freeRectangle

  (w, h) <- view standardPin <$> technology

  liftInteger $ width path =^ literal w <|> height path =^ literal h

  pure path


pinPolygon :: Pin -> LSC (Pin, IComponent)
pinPolygon pin = do

  path <- freeRectangle

  (w, h) <- view standardPin <$> technology

  liftInteger $ do
    width  path =^ literal w
    height path =^ literal h

  pure (pin, path)



setGateGeometry f (gate, rows) = getLayered f rows
  >>= \ path -> pure $ gate & geometry .~ pure path

setPinGeometry f (pin, rows) = getLayered f rows
  >>= \ path -> pure $ pin & ports .~ pure path

setNetGeometry f (net, edge) = sequence (getLayered f <$> edge)
  >>= \ path -> pure $ net & geometry .~ path

getLayered f path = Layered
  <$> f (path ^. l)
  <*> f (path ^. b)
  <*> f (path ^. r)
  <*> f (path ^. t)
  <*> pure (path ^. z)


disjointGates :: INodes -> LSC ()
disjointGates nodes = do
  sequence_
    [ disjoint p q
    | ((_, p), (_, q)) <- distinctPairs $ toList nodes
    ]


disjointNets :: IEdges -> LSC ()
disjointNets edges = do
  sequence_
    [ disjoint p q
    | ((_, as), (_, bs)) <- distinctPairs $ toList edges
    , p <- as
    , q <- bs
    ]



inside :: IComponent -> IComponent -> LSC ()
inside i o = do
  d <- lambda <$> technology
  liftInteger $ do
    view l i - view l o >=^ literal d
    view b i - view b o >=^ literal d
    view r o - view r i >=^ literal d
    view t o - view t i >=^ literal d


disjoint :: IComponent -> IComponent -> LSC ()
disjoint p q = do
  d <- lambda <$> technology
  let overlap = or [ x == y | x <- view z p, y <- view z q ]
  when overlap $ liftInteger
      $ view l q - view r p >=^ literal d
    <|> view l p - view r q >=^ literal d
    <|> view b p - view t q >=^ literal d
    <|> view b q - view t p >=^ literal d




placement :: IComponent -> IRing -> IPins -> LSC ()
placement area ring rim = do

  liftInteger $ do
    area ^. l =^ 0
    area ^. b =^ 0

  outer ring `inside` area

  d <- literal . lambda <$> technology
  sequence_
    [ liftInteger $ do
        path ^. l =^ area ^. l
        outer ring ^. l - path ^. l >=^ d
    | (pin, path) <- toList rim
    , pin ^. dir == Just In
    ]
  sequence_
    [ liftInteger $ do
        path ^. r =^ area ^. r
        path ^. r - outer ring ^. r >=^ d
    | (pin, path) <- toList rim
    , pin ^. dir == Just Out
    ]
  sequence_
    [ liftInteger $ do
        path ^. b >=^ inner ring ^. b
        path ^. t <=^ inner ring ^. t
    | (_, path) <- toList rim
    ]

  sequence_
    [ disjoint p q
    | ((_, p), (_, q)) <- distinctPairs $ toList rim
    ]


distinctPairs :: [a] -> [(a, a)]
distinctPairs (x : xs) = fmap (x, ) xs ++ distinctPairs xs
distinctPairs _ = []


gatePolygon :: NetGraph -> Gate -> LSC (Gate, IComponent)
gatePolygon _ gate
  | gate ^. geometry & not . null
  = gate ^. geometry <&> fmap literal
      & head
      & integrate [Metal2, Metal3]
      & (,) gate
      & pure

gatePolygon netlist gate = do

  path <- freeRectangle <&> integrate [Metal2, Metal3]

  standardDimensions <- lookupDimensions gate <$> technology

  abstractDimensions <- pure $ join $ lookup (gate ^. identifier) (netlist ^. subcells)
    <&> view (supercell . geometry)
    <&> fmap (width &&& height)
    <&> listToMaybe

  let dimensions = abstractDimensions <|> standardDimensions

  for_ dimensions $ \ (w, h) -> liftInteger $ do
    view r path - view l path =^ literal w
    view t path - view b path =^ literal h

  pure (gate, path)


freeRectangle :: LSC IComponent
freeRectangle = do

  area <- liftInteger $ Rect
    <$> general
    <*> general
    <*> general
    <*> general

  liftInteger $ do
    bound 0 100000 `mapM_` area
    width  area >=^ 0
    height area >=^ 0

  pure area


freeRing :: LSC IRing
freeRing = do

  p <- Rect
    <$> freeRectangle
    <*> freeRectangle
    <*> freeRectangle
    <*> freeRectangle

  liftInteger $ do

    p ^. l . l =^ p ^. b . l
    p ^. b . b =^ p ^. l . b

    p ^. l . l =^ p ^. t . l
    p ^. t . t =^ p ^. l . t

    p ^. r . r =^ p ^. b . r
    p ^. b . b =^ p ^. r . b

    p ^. r . r =^ p ^. t . r
    p ^. t . t =^ p ^. r . t

  pure p
