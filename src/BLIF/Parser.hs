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
blif = BLIF <$> many1 model <?> "blif"

model :: Parser Model
model = Model
  <$> modelName
  <*> inputList
  <*> outputList
  <*> clockList
  <*> many command
  <*  end_
  <?> "model"

modelName :: Parser ModelName
modelName = model_ *> ident <?> "model_name"

inputList :: Parser InputList
inputList = inputs_ *> many ident <|> pure [] <?> "decl_input_list"

outputList :: Parser OutputList
outputList = outputs_ *> many ident <|> pure [] <?> "decl_output_list"

clockList :: Parser ClockList
clockList = clock_ *> many ident <|> pure [] <?> "decl_clock_list"

command :: Parser Command
command
  =   LogicGate_Command <$> logicGate

logicGate :: Parser LogicGate
logicGate = names_ >> LogicGate
  <$> inputList
  <*> ident
  <*> singleOutputCover
  <?> "logic_gate"

singleOutputCover :: Parser SingleOutputCover
singleOutputCover = SingleOutputCover
  <$> many ((,) <$> inputPlane <*> outputPlane)
  <?> "single_output_cover"


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

ident :: Parser Ident
ident = maybeToken q
  where q (Tok_Ident t) = Just t
        q _ = Nothing

inputPlane :: Parser InputPlane
inputPlane = maybeToken q
  where q (Tok_InputPlane t) = Just t
        q _ = Nothing

outputPlane :: Parser OutputPlane
outputPlane = maybeToken q
  where q (Tok_InputPlane t) = Just t
        q _ = Nothing

p :: Token -> Parser ()
p t = maybeToken $ \r -> if r == t then Just () else Nothing
model_ = p Tok_Model
inputs_ = p Tok_Inputs
outputs_ = p Tok_Outputs
clock_ = p Tok_Clock
end_ = p Tok_End
names_ = p Tok_Names

