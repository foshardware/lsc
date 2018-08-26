{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module LSC.Exlining where

import Control.Applicative
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

    ((len, pos, _) : _) = isomorphicGates
    isomorphicGates
      = sortBy ( \ (l, _, x) (m, _, y) -> compare (y, m) (x, l))
      $ filter ( \ (_, _, score) -> score > 0)
      $ rescore nodes <$> maximalRepeatsDisjoint (hash . gateIdent) nodes k

    gate p = Gate (modelName netlist) (wires p) [] 0
    wires p = Map.assocs $ Map.fromList
      [ (i, v)
      | node <- toList $ slice (p - len) len nodes
      , (_, v) <- gateWires node
      , (i, _) <- maybeToList $ Map.lookup v closure
      ]

    (closure, netlist) = createSublist len pos top

    newGateVector = concat $ reverse
      [ head $ [ gate p `cons` slice p (q - p) nodes | p > 0 ] <> [ slice 0 q nodes ]
      | p <- fmap (+ len) pos <> pure 0
      | q <- length nodes : pos
      ]

exline _ netlist = netlist


createSublist :: Length -> [Position] -> Netlist -> (Closure, Netlist)
createSublist len pos@(p1 : _) (Netlist name (inputList, outputList, _) _ nodes edges)
  = (,) closure
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
      | (v, (i, (k, g))) <- Map.assocs closure
      , dir <- maybeToList $ Map.lookup v modelDirs <|> Map.lookup (gateIdent g, k) scopeDirs
      ]

    scope = slice p1 len nodes

    scopeDirs = directions edges
    modelDirs = Map.fromList $ fmap (, In) inputList <> fmap (, Out) outputList

    mask i = Gate gn [ (k, maybe v fst $ Map.lookup v closure) | (k, v) <- ws ] [] gi
      where Gate gn ws _ gi = scope ! i

    closure = Map.unions
      [ scopeWires inner `Map.intersection` (scopeWires outer `Map.union` model)
      | p <- pos
      , let inner = slice p len nodes
      , let outer = slice 0 p nodes <> slice (p + len) (length nodes - p - len) nodes
      , let model = Map.fromList [(w, (w, (w, Gate name [] [] 0))) | w <- inputList <> outputList]
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

    piece p = [ v | node <- toList $ slice p len nodes, (_, v) <- gateWires node ]


scopeWires :: Foldable f => f Gate -> Closure
scopeWires nodes = Map.fromList
  [ (v, (k <> showt i, (k, node)))
  | (i :: Int, node) <- [0..] `zip` toList nodes
  , (k, v) <- gateWires node
  ]


buildName :: (Functor f, Foldable f) => f Gate -> Identifier
buildName = showt . abs . hash . foldr mappend mempty . fmap gateIdent


directions :: Vector Net -> Map (Identifier, Identifier) Dir
directions edges = Map.fromList
  [ ((gateIdent gate, contact), pinDir pin)
  | Net cs _ <- toList edges
  , Contact gate contact pin <- cs
  ]


