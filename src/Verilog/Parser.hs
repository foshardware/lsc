{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Verilog.Parser where

import Control.Applicative hiding (Const, many, (<|>))
import Data.Maybe
import Data.Text (Text)
import Text.Parsec hiding (optional, noneOf)
import Text.Parsec.Pos
import Text.Parsec.String (GenParser)
import Prelude hiding (null)

import Verilog.Lexer
import Verilog.Syntax


type Parser = GenParser (Lexer Token) ()

parseVerilog :: Text -> Either ParseError Verilog
parseVerilog = parse ast [] . lexer []

ast :: Parser Verilog
ast = Verilog <$> many1 module'

module' :: Parser Module
module' = Module <$> file

-- --
--
maybeToken :: (Token -> Maybe a) -> Parser a
maybeToken test = token showT posT testT
  where
  showT (L _ t) = show t
  posT  (L x _) = pos2sourcePos x
  testT (L _ t) = test t
  pos2sourcePos (l, c) = newPos "" l c

noneOf :: [Token] -> Parser Token
noneOf ts = maybeToken $ \ r -> if r `elem` ts then Nothing else Just r

file :: Parser Text
file = maybeToken q
  where q (Tok_File t) = Just t
        q _ = Nothing

p :: Token -> Parser ()
p t = maybeToken $ \r -> if r == t then Just () else Nothing
module_ = p Tok_Module
semi = p Tok_Semi
lparen = p Tok_LParen
rparen = p Tok_RParen
comma = p Tok_Comma
assign_op = p Tok_AssignOp
assign = p Tok_Assign
endmodule = p Tok_Endmodule
wire = p Tok_Wire
reg = p Tok_Reg
end = p Tok_End
plusarg_reader = p Tok_PlusargReader

showRtl (Verilog x) = show $ length x
