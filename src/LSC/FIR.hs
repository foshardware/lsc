-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

module LSC.FIR where

import Data.Default
import Data.Text

import Language.FIRRTL.Lexer
import Language.FIRRTL.Parser
import Language.FIRRTL.Syntax

import LSC.Model


newtype FIR = FIR Circuit
  deriving (Eq, Show)

parseFIR :: Text -> Either ParseError FIR
parseFIR = fmap FIR . circuit . lexer []


firrtl :: FIR -> RTL
firrtl (FIR _) = def
