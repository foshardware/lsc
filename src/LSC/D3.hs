{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module LSC.D3 where

import Data.Aeson
import Data.Maybe
import qualified Data.Map as Map
import Data.ByteString.Lazy (ByteString)

import Verilog.Syntax


data D3Dag = D3Dag String [D3Dag]

instance ToJSON D3Dag where
  toJSON (D3Dag name children) = object ["name" .= name, "children" .= toJSON children]


newtype DAG a = DAG a

encodeVerilog :: Verilog -> ByteString
encodeVerilog = encode . DAG

instance ToJSON (DAG Verilog) where
  toJSON (DAG verilog) = toJSON (build root)
    where
    build node
      = D3Dag node
      $ fmap build
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

