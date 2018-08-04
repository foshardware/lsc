
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
  = LogicGate [Ident] SingleOutputCover
  | LibraryGate Ident FormalActualList
  | Attribute Ident StringLiteral
  | Parameter Ident Plane
  deriving (Eq, Show)

newtype SingleOutputCover = SingleOutputCover [Plane]
  deriving (Eq, Show)

type Plane = Text
type InputPlane = Plane
type OutputPlane = Plane

type LibraryGate = Command
type LogicGate = Command
type Attribute = Command
type Parameter = Command


type FormalActualList = [Assignment]

type Assignment = (Ident, Ident)

type StringLiteral = Text



