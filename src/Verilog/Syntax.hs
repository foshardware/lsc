
module Verilog.Syntax where

import Data.Text (Text)

data Verilog = Verilog [Module]
  deriving (Eq, Show)

data Module = Module Text
  deriving (Eq, Show)

type Ident = Text
