
module Verilog.Parser where

import Data.Text

import Language.Verilog.Parser
import Language.Verilog.Parser.Preprocess

import Verilog.Syntax


parseVerilog :: Text -> Verilog
parseVerilog = Verilog . parseFile [] "" . unpack

preprocessor :: Text -> String
preprocessor = preprocess [] "" . unpack
