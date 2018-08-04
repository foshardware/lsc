
module BLIF.Tokens
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
    = Tok_Model
    | Tok_Inputs
    | Tok_Outputs
    | Tok_Clock
    | Tok_End
    | Tok_Attr
    | Tok_Param
    | Tok_Subckt

    -- Logic gates
    | Tok_Names
    | Tok_Gate

    -- Operators
    | Tok_Assign

    -- Identifiers
    | Tok_Ident Text
    | Token Text

    -- Literals
    | Tok_InputPlane Text
    | Tok_OutputPlane Text
    | Tok_StringLiteral Text

  deriving (Eq, Show)
