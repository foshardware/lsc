{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module LSC.D3 where

import Data.Aeson
import Data.Map as Map
import Data.ByteString.Lazy (ByteString)

import Verilog.Syntax


data D3Dag = D3Dag String [D3Dag]

instance ToJSON D3Dag where
  toJSON (D3Dag name children) = object ["id" .= name, "children" .= toJSON children]


newtype DAG a = DAG a

encodeVerilog :: Verilog -> ByteString
encodeVerilog = encode . DAG

instance ToJSON (DAG Verilog) where
  toJSON (DAG verilog) = toJSON (verilogModuleHierarchy verilog)

verilogModuleHierarchy :: Verilog -> Map String [String]
verilogModuleHierarchy verilog = Map.fromList
  [ (moduleName m, moduleReferences m)
  | m <- modules verilog
  ]

