{-# LANGUAGE TypeFamilies, ParallelListComp #-}

module LSC.Exlining where

import Control.Monad
import Data.Foldable
import Data.Function
import Data.Hashable
import Data.List (sortBy)
import Data.Maybe
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector hiding
  ( replicate
  , foldl', null, toList, length, zip, foldr, elem, filter
  , take
  )
import TextShow

import LSC.Types
import LSC.SuffixTree


exline :: Int -> Netlist -> Netlist
exline k (Netlist name pins subs nodes edges)
  | not $ null isomorphicGates
  = Netlist name pins

  (Map.insert (modelName abstractNetlist) abstractNetlist subs)

  mempty -- nodes

  mempty -- edges

  where

    ((len, pos, _) : _) = isomorphicGates
    isomorphicGates
      = sortBy (\ (l, _, x) (m, _, y) -> compare (y, m) (x, l))
      $ filter ( \ (_, _, score) -> score > 0)
      $ rescore nodes <$> maximalRepeatsDisjoint (hash . gateIdent) nodes k

    scope = slice p1 len nodes
    p1 : _ = pos

    inputIdents  = lookupDirection [InOut,  In]
    outputIdents = lookupDirection [InOut, Out]

    lookupDirection f =
      [ ident
      | (ident, (contact, gate)) <- enumGates closure scope
      , dir <- maybeToList $ Map.lookup (gateIdent gate, contact) pinDirections
      , dir `elem` f
      ]
    pinDirections = Map.fromList
      [ ((gateIdent gate, contact), pinDir pin)
      | Net contacts _ <- toList edges
      , Contact gate contact pin <- contacts
      ]

    abstractNetlist
      = Netlist

      (buildName scope)

      (inputIdents, outputIdents, mempty)

      mempty

      mempty

      mempty

    abstractGate = Gate mempty mempty 0

    closure = Map.fromListWith Set.union
      [ (i, Set.singleton x)
      | p <- pos
      , let inner = slice p len nodes
      , let outer = slice 0 p nodes <> slice (p + len) (length nodes - p - len) nodes
      , (i, x) <- Map.elems $ scopeWires inner `Map.intersection` scopeWires outer
      ]

exline _ netlist = netlist


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


scopeWires :: Foldable f => f Gate -> Map Identifier (Int, Wire)
scopeWires nodes = Map.fromList
  [ (k, (i, x))
  | (i, node) <- [0..] `zip` toList nodes
  , (x, k) <- gateWires node
  ]


buildName :: (Functor f, Foldable f) => f Gate -> Identifier
buildName = showt . abs . hash . foldr mappend mempty . fmap gateIdent


enumGates
  :: Foldable f
  => Map Int (Set Identifier)
  -> f Gate
  -> [(Identifier, (Identifier, Gate))]
enumGates closure scope =
  [ (x <> showt i, (x, node))
  | (i, node) <- [0..] `zip` toList scope
  , (x, _) <- gateWires node
  , ref <- maybeToList $ Map.lookup i closure
  , x `elem` ref
  ]


