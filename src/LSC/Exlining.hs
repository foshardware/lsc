{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module LSC.Exlining where

-- | Software compilers use inlining which is grounded in the von Neumann architecture;
--   Dually, hardware compilers use exlining to identify an implicit module hierarchy.
--

import Control.Applicative
import Data.Default
import Data.Foldable hiding (concat)
import Data.Function (on)
import Data.Hashable
import Data.List (sortBy)
import Data.Maybe
import Data.Monoid
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector (Vector, slice, concat, (!), generate, cons)
import Data.Text (Text)
import Prelude hiding (concat)
import TextShow

import LSC.Types
import LSC.SuffixTree


exlineRounds :: Foldable f => f Int -> NetGraph -> NetGraph
exlineRounds xs netlist = foldr exline netlist xs


exline :: Int -> NetGraph -> NetGraph
exline k top@(NetGraph name pins subs nodes edges)
  | not $ null isomorphicGates
  = NetGraph name pins

  (Map.insert (modelName netlist) netlist subs)

  newGateVector

  edges

  where

    ((len, pos@(p1 : _), _) : _) = isomorphicGates
    isomorphicGates
      = sortBy ( \ (l, _, x) (m, _, y) -> compare (y, m) (x, l))
      $ filter ( \ (_, _, score) -> score > 0)
      $ rescore nodes <$> maximalRepeatsDisjoint (hash . gateIdent) nodes k

    gate p = Gate (modelName netlist) (wires p) (maps p) 0
    wires p = Map.fromList
      [ (wireName w, v)
      | node <- toList $ slice (p - len) len nodes
      , v <- toList (gateWires node)
      , w <- maybe [] pure $ Map.lookup v =<< Map.lookup (p - len) closures
      ]
    maps p = Map.fromList
      [ (v, u)
      | (offset, nodep) <- zip [0..] $ toList $ slice (p - len) len nodes
      , let node1 = nodes ! (p1 + offset)
      , (u, v) <- toList (gateWires nodep) `zip` toList (gateWires node1)
      ]

    (closures, netlist) = createSublist len pos top

    newGateVector = concat $ reverse
      [ head $ [ gate p `cons` slice p (q - p) nodes | p > 0 ] <> [ slice 0 q nodes ]
      | p <- fmap (+ len) pos <> pure 0
      | q <- length nodes : pos
      ]

exline _ netlist = netlist


createSublist :: Length -> [Position] -> NetGraph -> (Map Position Closure, NetGraph)
createSublist len pos@(p1 : _) (NetGraph name (inputList, outputList, _) _ nodes edges)
  = (,) closures
  $ NetGraph (name <> buildName scope)

  ( [ pin | (pin, dir) <- abstractPins, dir `elem` [In,  InOut] ]
  , [ pin | (pin, dir) <- abstractPins, dir `elem` [Out, InOut] ]
  , mempty
  )

  mempty

  (generate (length scope) mask)

  mempty

  where

    abstractPins = Map.assocs $ Map.fromList
      [ (wireName w, dir)
      | (v, w@(_, (k, g))) <- maybe [] Map.assocs $ Map.lookup p1 closures
      , dir <- maybe [] pure $ direction g k v edges <|> Map.lookup v modelDirs
      ] 

    modelDirs = Map.fromList $ fmap (, In) inputList <> fmap (, Out) outputList

    scope = slice p1 len nodes

    mask i = (scope ! i) { gateWires = lexicon <$> gateWires (scope ! i) } where
      lexicon v = maybe v wireName $ Map.lookup v =<< Map.lookup p1 closures

    -- keep scopes distinct by position in the gate vector
    closures = Map.fromList
      [ (p, scopeWires inner `Map.intersection` (scopeWires outer `Map.union` model))
      | p <- pos
      , let inner = slice p len nodes
      , let outer = slice 0 p nodes <> slice (p + len) (length nodes - p - len) nodes
      , let model = Map.fromList [(w, (def, (w, def))) | w <- inputList <> outputList]
      ]

createSublist _ _ netlist = (mempty, netlist)


rescore :: Vector Gate -> (Length, [Position], Int) -> (Length, [Position], Int)
rescore     _ (len, [p], score) = (len, [p], score)
rescore nodes (len, pos,     _) = (len, qos, len * length qos)

  where

    qos = filter (eqConfig maximalPosition) pos

    maximalPosition = snd $ maximumBy (compare `on` fst)
      [ (length [ q | q <- pos, eqConfig p q, p > q ], p)
      | p <- pos
      ]

    piece p = [ v | node <- toList $ slice p len nodes, v <- toList $ gateWires node ]

    eqConfig p q = p == q
      || length (wires p) == length (wires q)
      && toList (wires p) == toList (wires q)
      && fst (foldr conf (True, mempty) $ piece p `zip` piece q)

    conf (a, b) (s, ss) = (s && maybe s (b ==) (Map.lookup a ss), Map.insert a b ss)

    wires x = fmap fst $ scopeWires (inner x) `Map.intersection` scopeWires (outer x)
    inner x = slice x len nodes
    outer x = slice 0 x nodes <> slice (x + len) (length nodes - x - len) nodes


type Closure = Map Identifier (Int, (Wire, Gate))

-- | Creates a dictionary for all nets mapping its name to several details
--
scopeWires :: Foldable f => f Gate -> Closure
scopeWires nodes = Map.fromList $ reverse
  [ (v, (i, (k, node)))
  | (i :: Int, node) <- [0..] `zip` toList nodes
  , (k, v) <- Map.assocs $ gateWires node
  ]


wireName :: (Int, (Wire, Gate)) -> Text
wireName (i, (k, _)) = k <> showt i


buildName :: (Functor f, Foldable f) => f Gate -> Identifier
buildName = showt . abs . hash . foldr mappend mempty . fmap gateIdent


direction :: Gate -> Identifier -> Identifier -> Map Identifier Net -> Maybe Dir
direction gate contact net edges
  = pure . pinDir
  =<< lookup contact
  =<< Map.lookup gate . contacts
  =<< Map.lookup net edges


