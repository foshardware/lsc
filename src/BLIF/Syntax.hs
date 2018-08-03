
module BLIF.Syntax where

import Data.Text (Text)

data BLIF = BLIF [Model]
  deriving (Eq, Show)

data Model = Model ModelName InputList OutputList ClockList [Command]
  deriving (Eq, Show)

type ModelName = Text

type InputList = [Text]

type OutputList = [Text]

type ClockList = [Text]

data Command = Command [Text]
  deriving (Eq, Show)


