
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
  deriving (Eq, Show)

data LogicGate = LogicGate InputList Ident SingleOutputCover
  deriving (Eq, Show)

newtype SingleOutputCover = SingleOutputCover [(InputPlane, OutputPlane)]
  deriving (Eq, Show)

type InputPlane = Text

type OutputPlane = Text

