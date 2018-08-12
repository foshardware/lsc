
module LSC.Exlining where

import Data.List
import Data.Function

import LSC.Types

lcstr :: (a -> a -> Bool) -> [a] -> [a] -> [a]
lcstr p xs ys
  = maximumBy (compare `on` length)
  . concat
  $  [f xs' ys | xs' <- tails xs]
  ++ [f xs ys' | ys' <- drop 1 $ tails ys]
  where f xs ys = scanl g [] $ zip xs ys
        g z (x, y) = if p x y then z ++ [x] else []

findPattern :: (a -> a -> Bool) -> [a] -> [a]
findPattern p xs
  = lcstr p a b
  where (a, b) = splitAt (length xs `div` 2) xs

hierarchical :: Netlist -> [Gate]
hierarchical (Netlist nodes _)
  = findPattern p nodes
  where p g h = gateIdent g == gateIdent h
