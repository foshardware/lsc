{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Liberty.Parser where

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

import Liberty.Lexer
import Liberty.Syntax


type Parser = GenParser (Lexer Token) ()

parseAST :: Text -> Either ParseError Liberty
parseAST = parse ast [] . lexer []

ast :: Parser Liberty
ast = Liberty <$> some undefined