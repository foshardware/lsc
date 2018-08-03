{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module BLIF.Parser where

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

parseBLIF :: Text -> Either ParseError BLIF
parseBLIF = parse blif [] . lexer []

blif :: Parser BLIF
blif = BLIF <$> many1 model

model :: Parser Model
model = Model
  <$> (model_   *> modelName )
  <*> (inputs_  *> inputList )
  <*> (outputs_ *> outputList)
  <*> (clock_   *> clockList )
  <*> many command
  <*  end_

modelName :: Parser ModelName
modelName = ident

inputList :: Parser InputList
inputList = many ident

outputList :: Parser OutputList
outputList = many ident

clockList :: Parser ClockList
clockList = many ident

command :: Parser Command
command = Command <$> many ident


-----
--
-- think `optional` but for Either
eitherOr :: Parser a -> Parser b -> Parser (Either a b)
eitherOr a b = Left <$> a <|> Right <$> b

maybeToken :: (Token -> Maybe a) -> Parser a
maybeToken test = token showT posT testT
  where
  showT (L _ t) = show t
  posT  (L x _) = pos2sourcePos x
  testT (L _ t) = test t
  pos2sourcePos (l, c) = newPos "" l c

ident :: Parser Text
ident = maybeToken q
  where q (Tok_Ident t) = Just t
        q _ = Nothing

p :: Token -> Parser ()
p t = maybeToken $ \r -> if r == t then Just () else Nothing
model_ = p Tok_Model
inputs_ = p Tok_Inputs
outputs_ = p Tok_Outputs
clock_ = p Tok_Clock
end_ = p Tok_End

