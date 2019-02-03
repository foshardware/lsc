
module LSC.FIR where

import Data.Default
import Data.Text

import Language.FIRRTL.Lexer
import Language.FIRRTL.Parser
import Language.FIRRTL.Syntax

import LSC.Types


newtype FIR = FIR Circuit
  deriving (Eq, Show)

parseFIR :: Text -> FIR
parseFIR = FIR . circuit . lexer []


firrtl :: FIR -> RTL
firrtl (FIR _) = def
