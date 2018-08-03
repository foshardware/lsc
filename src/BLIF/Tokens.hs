
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

    -- Logic gates
    | Tok_Names

    -- Identifiers
    | Tok_Ident Text

    -- Literals
    | Tok_False
    | Tok_True

  deriving (Eq, Show)
