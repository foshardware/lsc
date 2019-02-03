
module LSC.FIR where

import Data.Text

import Language.FIRRTL.Lexer
import Language.FIRRTL.Parser
import Language.FIRRTL.Syntax


newtype FIR = FIR Circuit
  deriving (Eq, Show)

parseFIR :: Text -> FIR
parseFIR = FIR . circuit . lexer []
