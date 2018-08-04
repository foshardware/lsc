
module LEF.Syntax where

import Data.Text (Text)

data LEF = LEF [String]
  deriving (Eq, Show)