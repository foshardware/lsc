
module LSC.Numbers where

import Text.ParserCombinators.Parsec
import Data.Char (digitToInt)
import Control.Monad (liftM, ap)

floating2 :: Floating f => Bool -> CharParser st f
floating2 = liftM (either fromInteger id) . decFloat

floating3 :: Floating f => Bool -> CharParser st f
floating3 b = genFractAndExp 0 (fraction True) exponentFactor <|> floating2 b

decFloat :: (Integral i, Floating f) => Bool -> CharParser st (Either i f)
decFloat b = do
  n <- decimal
  option (Left n) $ liftM Right $ fractExp (toInteger n) b

fractExp :: Floating f => Integer -> Bool -> CharParser st f
fractExp i b = genFractExp i (fraction b) exponentFactor

genFractExp :: Floating f => Integer -> CharParser st f
  -> CharParser st (f -> f) -> CharParser st f
genFractExp i frac expo = case fromInteger i of
  f -> genFractAndExp f frac expo <|> liftM ($ f) expo

genFractAndExp :: Floating f => f -> CharParser st f
  -> CharParser st (f -> f) -> CharParser st f
genFractAndExp f frac = ap (liftM (flip id . (f +)) frac) . option id

exponentFactor :: Floating f => CharParser st (f -> f)
exponentFactor = oneOf "eE" >> extExponentFactor 10 <?> "exponent"

extExponentFactor :: Floating f => Int -> CharParser st (f -> f)
extExponentFactor base =
  liftM (flip (*) . exponentValue base) (ap sign (decimal <?> "exponent"))

exponentValue :: Floating f => Int -> Integer -> f
exponentValue base = (fromIntegral base **) . fromInteger

fractFract :: Fractional f => Integer -> Bool -> CharParser st f
fractFract i = genFractFract i . fraction

genFractFract :: Fractional f => Integer -> CharParser st f -> CharParser st f
genFractFract i = liftM (fromInteger i +)

fraction :: Fractional f => Bool -> CharParser st f
fraction b = baseFraction b 10 digit

baseFraction :: Fractional f => Bool -> Int -> CharParser st Char
  -> CharParser st f
baseFraction requireDigit base baseDigit = char '.' >>
  liftM (fractionValue base)
    ((if requireDigit then many1 else many) baseDigit <?> "fraction")
  <?> "fraction"

fractionValue :: Fractional f => Int -> String -> f
fractionValue base = uncurry (/)
  . foldl (\ (s, p) d ->
           (p * fromIntegral (digitToInt d) + s, p * fromIntegral base))
    (0, 1) . dropWhile (== '0') . reverse

int :: Integral i => CharParser st i
int = ap sign nat

sign :: Num a => CharParser st (a -> a)
sign = (char '-' >> return negate) <|> (optional (char '+') >> return id)

decimal :: Integral i => CharParser st i
decimal = number 10 digit

nat :: Integral i => CharParser st i
nat = zeroNumber <|> decimal

zeroNumber :: Integral i => CharParser st i
zeroNumber =
  char '0' >> (decimal <|> pure 0) <?> ""

number :: Integral i => Int -> GenParser tok st Char -> GenParser tok st i
number base baseDigit = do
  n <- liftM (numberValue base) (many1 baseDigit)
  seq n (return n)

numberValue :: Integral i => Int -> String -> i
numberValue base =
  foldl (\ x -> ((fromIntegral base * x) +) . fromIntegral . digitToInt) 0
