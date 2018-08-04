{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module LEF.Parser where

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

import LEF.Lexer
import LEF.Syntax


type Parser = GenParser (Lexer Token) ()

parseLEF :: Text -> Either ParseError LEF
parseLEF = parse lef [] . lexer []

lef :: Parser LEF
lef = LEF <$> some undefined
