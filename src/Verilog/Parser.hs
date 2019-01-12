
module Verilog.Parser where

import Data.Text

import Language.Verilog.Parser

import Verilog.Syntax


parseVerilog :: Text -> Verilog
parseVerilog = Verilog . parseFile [] ""

preprocessor :: Text -> Text
preprocessor = preprocess [] ""
