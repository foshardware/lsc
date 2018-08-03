
module BLIF.Syntax where

import Data.Text (Text)

data BLIF = BLIF [String]
  deriving (Eq, Show)