
module BLIF.Syntax where

import Data.Text (Text)

type Ident = Text

data BLIF = BLIF [Model]
  deriving (Eq, Show)

data Model = Model ModelName InputList OutputList ClockList [Command]
  deriving (Eq, Show)

type ModelName = Ident

type InputList = [Ident]

type OutputList = [Ident]

type ClockList = [Ident]

data Command
  = LogicGate_Command LogicGate
  | LibraryGate_Command LibraryGate
  | Attribute_Command Attribute
  deriving (Eq, Show)

data LogicGate = LogicGate [Ident] SingleOutputCover
  deriving (Eq, Show)

newtype SingleOutputCover = SingleOutputCover [Plane]
  deriving (Eq, Show)

type Plane = Text
type InputPlane = Plane
type OutputPlane = Plane

data LibraryGate = LibraryGate Ident FormalActualList
  deriving (Eq, Show)

type FormalActualList = [Assignment]

type Assignment = (Ident, Ident)

data Attribute = Attribute Ident StringLiteral
  deriving (Eq, Show)

type StringLiteral = Text

