
module Verilog.Syntax where

import Data.Maybe

import Language.Verilog.AST


newtype Verilog = Verilog { modules :: [Module] }
  deriving (Eq, Show)

moduleName :: Module -> String
moduleName (Module name _ _) = name

moduleReferences :: Module -> [String]
moduleReferences (Module _ _ items) = catMaybes $ instanceName <$> items

instanceName :: ModuleItem -> Maybe String
instanceName (Instance name _ _ _) = Just name
instanceName _ = Nothing
