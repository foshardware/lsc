{-# LANGUAGE TypeFamilies, ParallelListComp #-}

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
  , take
  )
import Prelude hiding (concat)
import TextShow

import LSC.Types
import LSC.SuffixTree


exline :: Int -> Netlist -> Netlist
exline k (Netlist name pins subs nodes edges)
  | not $ null isomorphicGates
  = Netlist name pins

  (Map.insert (modelName abstractNetlist) abstractNetlist subs)

  (foldr (<>) mempty $ reverse $
    [ case p of
        0 -> slice 0 q nodes
        _ -> abstractGate (slice (p - len) len nodes) `cons` slice p (q - p) nodes
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

    scope = slice p1 len nodes
    p1 : _ = pos

    inputIdents  = lookupDirection [InOut,  In]
    outputIdents = lookupDirection [InOut, Out]

    lookupDirection f =
      [ ident
      | ((contact, _), (ident, gate)) <- gateAssignments closure scope
      , dir <- maybeToList $ Map.lookup (gateIdent gate, contact) pinDirections
      , dir `elem` f
      ]
    pinDirections = Map.fromList
      [ ((gateIdent gate, contact), pinDir pin)
      | Net cs _ <- toList edges
      , Contact gate contact pin <- cs
      ]

    abstractNetlist
      = Netlist (buildName scope) (inputIdents, outputIdents, mempty) mempty

      ( generate (length scope) $ \ i ->
         let gate = scope ! i
         in gate
            { gateWires =
              [ (j, v)
              | (j, v) <- gateWires gate
              -- , a <- maybeToList $ Nothing
              , ref <- maybeToList $ Map.lookup i closure
              ]
            } )

      mempty

    abstractGate s
      = Gate (buildName scope)
      [ (ident, contact) | ((_, contact), (ident, gate)) <- gateAssignments closure s ]
      (length nodes + 1)

    closure = Map.fromListWith Map.union
      [ (i, Map.singleton j o)
      | p <- pos
      , let inner = slice p len nodes
      , let outer = slice 0 p nodes <> slice (p + len) (length nodes - p - len) nodes
      , (o, (i, j)) <- Map.assocs $ scopeWires inner `Map.intersection` scopeWires outer
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
  [ (v, (i, k))
  | (i, node) <- [0..] `zip` toList nodes
  , (k, v) <- gateWires node
  ]


buildName :: (Functor f, Foldable f) => f Gate -> Identifier
buildName = showt . abs . hash . foldr mappend mempty . fmap gateIdent


gateAssignments
  :: Foldable f
  => Map Int (Map Identifier Identifier)
  -> f Gate
  -> [((Identifier, Identifier), (Identifier, Gate))]
gateAssignments closure scope =
  [ ((k, v), (k <> showt i, node))
  | (i, node) <- [0..] `zip` toList scope
  , (k, v) <- gateWires node
  , ref <- maybeToList $ Map.lookup i closure
  , Map.member k ref
  ]


