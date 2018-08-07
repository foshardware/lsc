{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Liberty.Parser where

import Control.Applicative hiding (Const)
import Data.Text (Text)
import Text.Parsec
import Text.Parsec.String (GenParser)
import Prelude hiding (null)

import Liberty.Lexer
import Liberty.Syntax


type Parser = GenParser (Lexer Token) ()

parseAST :: Text -> Either ParseError Liberty
parseAST = parse ast [] . lexer []

ast :: Parser Liberty
ast = Liberty <$> some undefined