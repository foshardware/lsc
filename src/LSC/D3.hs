{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module LSC.D3 where

import Data.Aeson
import Data.Maybe
import qualified Data.Map as Map
import Data.ByteString.Lazy (ByteString)

import Verilog.Syntax


data D3Dag = D3Dag Integer String [D3Dag]

instance ToJSON D3Dag where
  toJSON (D3Dag n name children) = object
    [ "name" .= unwords [show n, name]
    , "children" .= toJSON children
    ]


newtype DAG a = DAG a

encodeVerilog :: Verilog -> ByteString
encodeVerilog = encode . DAG

instance ToJSON (DAG Verilog) where
  toJSON (DAG verilog) = toJSON $ build (root, 1)

    where

    build (node, n)
      = D3Dag n node
      $ fmap build
      $ count
      $ concat $ maybeToList
      $ Map.lookup node dependencies

    root = head $
      [ name
      | name <- Map.keys dependencies
      , isNothing $ Map.lookup name dependents
      ] ++ (Map.keys dependencies)

    dependencies = Map.fromList
      [ (moduleName m, moduleReferences m)
      | m <- modules verilog
      ]

    dependents = Map.fromListWith (++)
      [ (child, [moduleName m])
      | m <- modules verilog
      , child <- moduleReferences m
      ]

    count xs = Map.assocs $ Map.fromListWith (+) [ (x, 1) | x <- xs ]
