{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ParallelListComp #-}

module LSC.Integer where

import Control.Arrow
import Control.Applicative
import Control.Monad

import Control.Lens hiding (inside)
import Data.Foldable
import Data.Maybe
import Data.Text (unpack)
import Data.Map (lookup)

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


  nodes <- sequence $ netlist ^. gates <&> gatePolygon netlist

  disjointGates nodes

  result <- minimizeInteger

  pure $ netlist &~ do
    gates .= fmap (setGateGeometry result) nodes


setGateGeometry :: Result -> (Gate, IComponent) -> Gate
setGateGeometry f (gate, path) = gate &~ do
  geometry .= maybe mempty pure (sequence $ f <$> path)


disjointGates :: INodes -> LSC ()
disjointGates nodes = do
  sequence_
    [ disjoint p q
    | ((_, p), (_, q)) <- distinctPairs $ toList nodes
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
    objective $ sum area
    bound 0 100000 `mapM_` area
    width  area >=^ 0
    height area >=^ 0

  pure area
