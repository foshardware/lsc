{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module LSC.Exlining where

-- | Software compilers use inlining which is grounded in the von Neumann architecture;
--   Dually, hardware compilers use exlining to identify an implicit module hierarchy.
--

import Control.Applicative
import Control.Monad.State
import Control.Parallel (par)
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
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Prelude hiding (concat)
import TextShow

import LSC.Types
import LSC.SuffixTree


type Exlining = State (SuffixTree Gate)


exlineDeep :: [Int] -> NetGraph -> NetGraph
exlineDeep = exlineDeepWithEscapeHatch $ const True


exlineDeepWithEscapeHatch :: (String -> Bool) -> [Int] -> NetGraph -> NetGraph
exlineDeepWithEscapeHatch escape _ netlist
  | escape $ unpack $ modelName netlist
  = netlist 
exlineDeepWithEscapeHatch escape ks top@(NetGraph name pins subs nodes edges)
  = netlist
  where
    netlist = deep `par` exline_ ks (NetGraph name pins deep nodes edges)
    deep = Map.fromAscList $ mapLSC
      [ (k, exlineDeepWithEscapeHatch escape ks v) | (k, v) <- Map.toAscList subs ]


exline_ :: [Int] -> NetGraph -> NetGraph
exline_ ks netlist = evalState
  (exline ks netlist)
  (constructSuffixTree (hash . gateIdent) (gateVector netlist))


exline :: [Int] -> NetGraph -> Exlining NetGraph
exline [] top = pure top
exline (k : ks) top@(NetGraph name pins subs nodes edges) = do

  suffixTree <- get

  let ((len, pos@(p1 : _), _) : _) = isomorphicGates suffixTree

      (closures, netlist) = createSublist len pos top

      gate p = Gate (modelName netlist) mempty (wires p) (maps p) 0

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

      newGateVector = concat $ reverse
        [ head $ [ gate p `cons` slice p (q - p) nodes | p > 0 ] <> [ slice 0 q nodes ]
        | p <- fmap (+ len) pos <> pure 0
        | q <- length nodes : pos
        ]

  put $ divideSuffixTree len pos (hash $ modelName netlist) newGateVector suffixTree

  if null $ isomorphicGates suffixTree
    then pure top
    else exline ks $ NetGraph
           name pins
           (Map.insert (modelName netlist) netlist subs)
           newGateVector
           edges
  where

    isomorphicGates suffixTree
      = sortBy ( \ (l, _, x) (m, _, y) -> compare (y, m) (x, l))
      $ take 4
      $ filter ( \ (l, p : _, _) -> primitive `all` slice p l nodes)
      $ filter ( \ (_, _, score) -> score > 0)
      $ fmap (rescore nodes)
      $ maximalRepeatsDisjoint suffixTree (hash . gateIdent) k

    primitive g = name /= T.take (T.length name) (gateIdent g)


createSublist :: Length -> [Position] -> NetGraph -> (Map Position Closure, NetGraph)
createSublist len pos@(p1 : _) (NetGraph name (inputList, outputList, _) _ nodes edges)
  = (,) closures
  $ NetGraph (name <> buildName scope)

  (newInputList, newOutputList, mempty)

  mempty

  newGateVector

  newNetMapping

  where

    newInputList  = [ pin | (pin, dir) <- abstractPins, dir `elem` [In,  InOut] ]
    newOutputList = [ pin | (pin, dir) <- abstractPins, dir `elem` [Out, InOut] ]

    newGateVector = generate (length scope) mask

    mask i = (scope ! i) { gateWires = lexicon <$> gateWires (scope ! i) } where
      lexicon v = maybe v wireName $ Map.lookup v =<< Map.lookup p1 closures

    newNetMapping = Map.fromList
      [ (w, net { netIdent = w })
      | (k, net) <- Map.assocs edges
      , w <- maybe [k] pure $ Map.lookup k scopeNets
      ]
    scopeNets = Map.fromList
      [ (v, w)
      | i <- [ 0 .. len - 1 ]
      , (v, w) <- Map.elems (gateWires $ scope ! i) `zip` Map.elems (gateWires $ mask i)
      ]

    abstractPins = Map.assocs $ Map.fromList
      [ (wireName w, dir)
      | (v, w@(_, (k, g))) <- maybe [] Map.assocs $ Map.lookup p1 closures
      , dir <- maybe [] pure $ direction g k v edges <|> Map.lookup v modelDirs
      ]

    modelDirs = Map.fromList $ fmap (, In) inputList <> fmap (, Out) outputList

    scope = slice p1 len nodes

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
buildName = showt . abs . hash . foldMap ( \ g -> foldMap id (gateWires g) <> gateIdent g)


direction :: Gate -> Identifier -> Identifier -> Map Identifier Net -> Maybe Dir
direction gate contact net edges
  = pure . pinDir
  =<< lookup contact
  =<< Map.lookup gate . contacts
  =<< Map.lookup net edges


