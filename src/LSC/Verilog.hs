-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

module LSC.Verilog where

import Data.Maybe
import Data.Text

import Language.Verilog.AST
import Language.Verilog.Parser


newtype Verilog = Verilog { modules :: [Module] }
  deriving (Eq, Show)

moduleName :: Module -> String
moduleName (Module name _ _) = name

moduleReferences :: Module -> [String]
moduleReferences (Module _ _ items) = catMaybes $ instanceName <$> items

instanceName :: ModuleItem -> Maybe String
instanceName (Instance name _ _ _) = Just name
instanceName _ = Nothing



parseVerilog :: Text -> Verilog
parseVerilog = Verilog . parseFile [] ""

preprocessor :: Text -> Text
preprocessor = preprocess [] ""

