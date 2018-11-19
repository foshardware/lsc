{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Verilog.Parser where

import Control.Applicative hiding (Const, many, (<|>))
import Data.Text (Text)
import Text.Parsec
import Text.Parsec.Pos
import Text.Parsec.String (GenParser)
import Prelude hiding (null)

import Verilog.Lexer
import Verilog.Syntax


type Parser = GenParser (Lexer Token) ()

parseAST :: Text -> Either ParseError Verilog
parseAST = parse ast [] . lexer []

ast :: Parser Verilog
ast = Verilog <$> many1 module'

module' :: Parser Module
module' = module_ >> Module
  <$> ident
  <*> between lparen rparen (wireDeclaration `sepBy` comma)
  <*> many expression
  <?> "module"

expression :: Parser Expression
expression = (moduleReference <|> incomprehensible) <* semi
  <?> "expression"

moduleReference :: Parser Expression
moduleReference = ModuleReference
  <$> ident
  <*> ident
  <*> between lparen rparen (wireDeclaration `sepBy` comma)
  <*  semi
  <?> "module_reference"

wireDeclaration :: Parser WireDeclaration
wireDeclaration = WireDeclaration () <$ incomprehensible

incomprehensible :: Parser Expression
incomprehensible = Incomprehensible () <$ many anyToken

-- --
--
maybeToken :: (Token -> Maybe a) -> Parser a
maybeToken test = token showT posT testT
  where
  showT (L _ t) = show t
  posT  (L x _) = pos2sourcePos x
  testT (L _ t) = test t
  pos2sourcePos (l, c) = newPos "" l c

ident :: Parser Ident
ident = maybeToken q
  where q (Tok_Ident t) = Just t
        q _ = Nothing

p :: Token -> Parser ()
p t = maybeToken $ \r -> if r == t then Just () else Nothing
module_ = p Tok_Module
semi = p Tok_Semi
lparen = p Tok_LParen
rparen = p Tok_RParen
comma = p Tok_Comma

