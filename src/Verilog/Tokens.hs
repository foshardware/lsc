
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
    | Tok_Assign
    | Tok_AssignOp
    | Tok_Ident Text
    | Tok_LParen
    | Tok_RParen
    | Tok_Wire
    | Tok_Reg
    | Tok_PlusargReader
    | Tok_End
    | Tok_Blackhole Text
    | Tok_File Text
  deriving (Eq, Show)
