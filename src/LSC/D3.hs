{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module LSC.D3 where

import Data.Aeson
import Data.Char
import Data.Foldable (toList)
import Data.Maybe
import Data.Text (unpack, isPrefixOf)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.ByteString.Lazy (ByteString)

import BLIF.Syntax      as BLIF
import Verilog.Syntax   as Verilog
import LSC.Types        as LSC


data D3Dag = D3Dag Integer String [D3Dag]

instance ToJSON D3Dag where
  toJSON (D3Dag n name children) = object
    [ "name" .= unwords [show n, name]
    , "children" .= toJSON children
    ]


newtype DAG a = DAG a

encodeVerilog :: Verilog -> ByteString
encodeVerilog = encode . DAG

encodeBLIF :: BLIF -> ByteString
encodeBLIF = encode . DAG

encodeNetGraph :: NetGraph -> ByteString
encodeNetGraph = encode . DAG


instance ToJSON (DAG BLIF) where
  toJSON (DAG blif) = toJSON $ object
    [ "tree" .= tree
    , "stages" .= stages
    ]

    where

    tree = build dependencies (findRoot dependencies dependents, 1)

    (_, stages) = eval tree

    dependencies = Map.fromList
      [ (unpack $ BLIF.modelName m, fmap unpack $ filter (not . primitive) $ subcircuits m)
      | m <- models blif
      ]

    dependents = Map.fromListWith (++)
      [ (unpack child, [unpack $ BLIF.modelName m])
      | m <- models blif
      , child <- subcircuits m
      , not $ primitive child
      ]


instance ToJSON (DAG Verilog) where
  toJSON (DAG verilog) = toJSON $ object [ "tree" .= tree, "stages" .= stages ]

    where

    tree = build dependencies (findRoot dependencies dependents, 1)

    (_, stages) = eval tree

    dependencies = Map.fromList
      [ (moduleName m, moduleReferences m)
      | m <- modules verilog
      ]

    dependents = Map.fromListWith (++)
      [ (child, [moduleName m])
      | m <- modules verilog
      , child <- moduleReferences m
      ]


instance ToJSON (DAG NetGraph) where
  toJSON (DAG netlist) = toJSON $ object [ "tree" .= tree, "stages" .= stages ]

    where

    tree = build dependencies (unpack $ LSC.modelName netlist, 1)

    (_, stages) = eval tree

    dependencies
      = Map.fromList
      $ ( unpack $ LSC.modelName netlist
        , fmap unpack $ filter (not . primitive) $ fmap gateIdent $ toList $ gateVector netlist
        )
      : [ ( unpack $ LSC.modelName m
          , fmap unpack $ filter (not . primitive) $ fmap gateIdent $ toList $ gateVector m
          )
        | m <- getLeaves netlist
        ]


type Leaves = Map String Integer

eval :: D3Dag -> ([D3Dag], [Leaves])
eval tree
  = unzip
  $ take 32
  $ takeWhile ( \ (D3Dag _ _ xs, _) -> not $ null xs)
  $ iterate (\ (t, ls) -> let s = cut ls t in (s, leaves s)) (tree, leaves tree)

cut :: Leaves -> D3Dag -> D3Dag
cut ls (D3Dag n k xs)
  = D3Dag n k
  $ cut ls <$> [ d | d@(D3Dag _ x _) <- xs, isNothing $ Map.lookup x ls ]

leaves :: D3Dag -> Leaves
leaves (D3Dag n k []) = Map.singleton k n
leaves (D3Dag _ _ xs) = Map.unionsWith (+) (leaves <$> xs)


primitive x = any (`isPrefixOf` x)
  ["BUF", "AND", "AOI", "NOR", "OAI", "XNOR", "MUX", "INV", "NAND", "NOR", "OR", "XOR", "DFF"]


count :: Ord a => [a] -> [(a, Integer)]
count xs = Map.assocs $ Map.fromListWith (+) [ (x, 1) | x <- xs ]


type Dependencies = Map String [String]

type Dependents   = Map String [String]


findRoot :: Dependencies -> Dependents -> String
findRoot dependencies dependents = head $
  [ name
  | name <- Map.keys dependencies
  , isNothing $ Map.lookup name dependents
  ] ++ (Map.keys dependencies)

build :: Dependencies -> (String, Integer) -> D3Dag
build dependencies (node, n)
  = D3Dag n node
  $ fmap (build dependencies)
  $ count
  $ concat $ maybeToList
  $ Map.lookup node dependencies
