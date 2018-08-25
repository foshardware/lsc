{-# LANGUAGE TypeFamilies, ParallelListComp, ScopedTypeVariables #-}

module LSC.Exlining where

import Data.Foldable hiding (concat)
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


exline :: Int -> Netlist -> Netlist
exline k top@(Netlist name pins subs nodes edges)
  | not $ null isomorphicGates
  = Netlist name pins

  (Map.insert (modelName netlist) netlist subs)

  (concat $ reverse
    [ head $ [ gate `cons` slice p (q - p) nodes | p > 0 ] <> [ slice 0 q nodes ]
    | p <- fmap (+ len) pos <> pure 0
    | q <- length nodes : pos
    ])

  edges

  where

    ((len, pos, _) : _) = isomorphicGates
    isomorphicGates
      = sortBy ( \ (l, _, x) (m, _, y) -> compare (y, m) (x, l))
      $ filter ( \ (_, _, score) -> score > 0)
      $ rescore nodes <$> maximalRepeatsDisjoint (hash . gateIdent) nodes k

    gate = Gate mempty mempty 0
    netlist = createSublist len pos top

exline _ netlist = netlist


createSublist :: Length -> [Position] -> Netlist -> Netlist
createSublist _ [] netlist = netlist

createSublist len pos@(p1 : _) (Netlist name _ _ nodes edges) = Netlist

  (name <> buildName scope)

  ( [ pin | (pin, dir) <- pins, dir `elem` [In,  InOut] ]
  , [ pin | (pin, dir) <- pins, dir `elem` [Out, InOut] ]
  , mempty
  )

  mempty

  (generate (length scope) mask)

  mempty

  where

    input  i = True
    output i = True

    pins =
      [ (i, dir)
      | (i, (k, g)) <- Map.elems closure
      , dir <- maybeToList $ Map.lookup (gateIdent g, k) dirs
      ]

    scope = slice p1 len nodes

    dirs = directions edges

    mask i = Gate gn [ (k, maybe v fst $ Map.lookup v closure) | (k, v) <- ws ] gi
      where Gate gn ws gi = scope ! i

    closure = Map.fromList
      [ (v, (i, (k, g)))
      | p <- pos
      , let inner = slice p len nodes
      , let outer = slice 0 p nodes <> slice (p + len) (length nodes - p - len) nodes
      , (v, (i, (k, g))) <- Map.assocs $ scopeWires inner `Map.intersection` scopeWires outer
      ]


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


scopeWires :: Foldable f => f Gate -> Map Identifier (Identifier, (Wire, Gate))
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


