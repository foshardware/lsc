
module Verilog.Syntax where

import Data.Text (Text)

data Verilog = Verilog [Module]
  deriving (Eq, Show)

data Module = Module Ident [WireDeclaration] [Expression]
  deriving (Eq, Show)

data WireDeclaration = WireDeclaration ()
  deriving (Eq, Show)

data Expression
  = ModuleReference Ident Ident [WireDeclaration]
  | Incomprehensible ()
  deriving (Eq, Show)

type Ident = Text
