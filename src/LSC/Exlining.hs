{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module LSC.Exlining where

import Control.Applicative
import Data.Default
import Data.Foldable hiding (concat)
import Data.Hashable
import Data.List (sortBy)
import Data.Maybe
import Data.Monoid
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector hiding
  ( replicate
  , foldl', foldl, foldr
  , elem, filter
  , null, length
  , toList, zip
  , reverse
  , head
  , take
  )
import Prelude hiding (concat)
import TextShow

import LSC.Types
import LSC.SuffixTree


type Closure = Map Identifier (Identifier, (Wire, Gate))


exlineRounds :: Foldable f => f Int -> Netlist -> Netlist
exlineRounds xs netlist = foldr exline netlist xs


exline :: Int -> Netlist -> Netlist
exline k top@(Netlist name pins subs nodes edges)
  | not $ null isomorphicGates
  = Netlist name pins

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
      [ (i, v)
      | node <- toList $ slice (p - len) len nodes
      , v <- toList (gateWires node)
      , (i, _) <- maybe [] pure $ Map.lookup v =<< Map.lookup (p - len) closures
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


createSublist :: Length -> [Position] -> Netlist -> (Map Position Closure, Netlist)
createSublist len pos@(p1 : _) (Netlist name (inputList, outputList, _) _ nodes edges)
  = (,) closures
  $ Netlist (name <> buildName scope)

  ( [ pin | (pin, dir) <- abstractPins, dir `elem` [In,  InOut] ]
  , [ pin | (pin, dir) <- abstractPins, dir `elem` [Out, InOut] ]
  , mempty
  )

  mempty

  (generate (length scope) mask)

  mempty

  where

    abstractPins = Map.assocs $ Map.fromList
      [ (i, dir)
      | (v, (i, (k, g))) <- maybe [] Map.assocs $ Map.lookup p1 closures
      , dir <- maybe [] pure $ Map.lookup v modelDirs <|> Map.lookup (gateIdent g, k) scopeDirs
      ]

    scope = slice p1 len nodes

    scopeDirs = directions edges
    modelDirs = Map.fromList $ fmap (, In) inputList <> fmap (, Out) outputList

    mask i = (scope ! i) { gateWires = lexicon <$> gateWires (scope ! i) } where
      lexicon v = maybe v fst $ Map.lookup v $ Map.unions (toList closures)

    closures = Map.fromList
      [ (p, scopeWires inner `Map.intersection` (scopeWires outer `Map.union` model))
      | p <- pos
      , let inner = slice p len nodes
      , let outer = slice 0 p nodes <> slice (p + len) (length nodes - p - len) nodes
      , let model = Map.fromList [(w, (w, (w, def))) | w <- inputList <> outputList]
      ]

createSublist _ _ netlist = (mempty, netlist)


rescore :: Vector Gate -> (Length, [Position], Int) -> (Length, [Position], Int)
rescore     _ (len, [p], score) = (len, [p], score)
rescore nodes (len, pos,     _) = (len, qos, len * length qos)

  where

    qos = foldr configs [] pos

    configs p [] = [p]
    configs p (q : ps)
      | fst $ foldr conf (True, mempty) $ piece p `zip` piece q
      = p : q : ps
    configs _ ps = ps

    conf (a, b) (s, ss) = (s && maybe s (b ==) (Map.lookup a ss), Map.insert a b ss)

    piece p = [ v | node <- toList $ slice p len nodes, v <- toList $ gateWires node ]


scopeWires :: Foldable f => f Gate -> Closure
scopeWires nodes = Map.fromList
  [ (v, (k <> showt i, (k, node)))
  | (i :: Int, node) <- [0..] `zip` toList nodes
  , (k, v) <- Map.assocs $ gateWires node
  ]


buildName :: (Functor f, Foldable f) => f Gate -> Identifier
buildName = showt . abs . hash . foldr mappend mempty . fmap gateIdent


directions :: Vector Net -> Map (Identifier, Identifier) Dir
directions edges = Map.fromList
  [ ((gateIdent gate, contact), pinDir pin)
  | Net cs _ <- toList edges
  , Contact gate contact pin <- cs
  ]


