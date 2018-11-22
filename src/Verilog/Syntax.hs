
module Verilog.Syntax where

import Language.Verilog.AST

newtype Verilog = Verilog [Module]
  deriving (Eq, Show)
