
module Verilog.Tokens
    ( Lexer (..)
    , Pos
    , Token (..)
    ) where

import Data.Text (Text)

data Lexer a = L Pos a
  deriving (Show, Eq)

type Pos = (Int, Int)

data Token
    -- Keywords
    = Tok_Module
    | Tok_Endmodule
    | Tok_Semi
    | Tok_Comma
    | Tok_Ident Text
    | Tok_LParen
    | Tok_RParen
  deriving (Eq, Show)
