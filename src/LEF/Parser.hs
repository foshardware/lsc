{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module LEF.Parser where

import Control.Applicative (optional)
import Control.Monad
import Data.Char
import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec hiding (option)
import Text.Parsec.String (GenParser)
import Text.Parsec.Combinator hiding (option)
import Text.Parsec.Pos
import Text.ParserCombinators.Parsec.Number
import Prelude hiding (null)

import LEF.Lexer
import LEF.Syntax


type Parser = GenParser (Lexer Token) ()

parseLEF :: Text -> Either ParseError LEF
parseLEF = parse lef [] . lexer []

lef :: Parser LEF
lef = LEF
  <$> many1 option
  <*> many layer
  <*> many via
  <*> many viaRule
  <*> many site
  <*> many1 macro
  <*  endLibrary
  <?> "lef"

option :: Parser Option
option
  =   version
  <|> cases
  <|> bitChars
  <|> divideChars
  <|> units
  <|> useMiNamespacing
  <|> clearanceMeasure
  <|> manufacturingGrid
  <?> "option"

version :: Parser Option
version = version_ >> Version <$> double <?> "version"

cases :: Parser Option
cases = namescasesensitive_ >> Cases <$> ident <?> "cases"

bitChars :: Parser Option
bitChars = busbitchars_ >> BitChars <$> ident <?> "bit_chars"

divideChars :: Parser Option
divideChars = dividerchar_ >> DivideChar <$> ident <?> "divide_char"

units :: Parser Option
units = units_ >> Units <$> databaseList <* end_ <* units_ <?> "units"

databaseList :: Parser DatabaseList
databaseList = database_ >> microns_ >> DatabaseList <$> integer <?> "database_list"



layer :: Parser Layer
layer = Layer
  <$> layerName
  <*> many layerOption
  <?> "layer"

layerName :: Parser LayerName
layerName = layer_ *> ident <?> "layer_name"

layerOption :: Parser LayerOption
layerOption
  =   Type        <$> (type_        *> ident ) 
  <|> Spacing     <$> (spacing_     *> double)
  <|> Direction   <$> (direction_   *> ident )
  <|> Pitch       <$> (pitch_       *> double)
  <|> Offset      <$> (offset_      *> double)
  <|> Width       <$> (width_       *> double)
  <|> Resistance  <$> (resistance_  *> ident ) <*> double
  <|> Capacitance <$> (capacitance_ *> ident ) <*> double
  <|> EdgeCapacitance <$> (capacitance_ *> double)
  <?> "layer_option"

manufacturingGrid = undefined
clearanceMeasure = undefined
useMiNamespacing = undefined
macro = undefined
viaRule = undefined
via = undefined
site = undefined





endLibrary :: Parser ()
endLibrary = end_ *> library_

--------
----

double :: Parser Double
double = either (fail . show) pure . parse (floating3 False) "double" . T.unpack =<< ident

integer :: Parser Integer
integer = either (fail . show) pure . parse int "integer" . T.unpack =<< ident

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
end_ = p Tok_End
library_ = p Tok_Library
version_ = p Tok_Version
capacitance_ = p Tok_Capacitance
resistance_ = p Tok_Resistance
width_ = p Tok_Width
offset_ = p Tok_Offset
pitch_ = p Tok_Pitch
direction_ = p Tok_Direction
spacing_ = p Tok_Spacing
type_ = p Tok_Type
layer_ = p Tok_Layer
units_ = p Tok_Units
dividerchar_ = p Tok_DividerChar
microns_ = p Tok_Microns
database_ = p Tok_Database
busbitchars_ = p Tok_BusBitChars
namescasesensitive_ = p Tok_Namescasesensitive



