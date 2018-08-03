{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module BLIF.Parser where

import Control.Applicative hiding (Const)
import Control.Monad
import Data.Char
import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.String (GenParser)
import Text.Parsec.Combinator
import Text.Parsec.Pos
import Prelude hiding (null)

import BLIF.Lexer
import BLIF.Syntax


type Parser = GenParser (Lexer Token) ()

parseAST :: Text -> Either ParseError BLIF
parseAST = parse ast [] . lexer []

ast :: Parser BLIF
ast = BLIF <$> some undefined