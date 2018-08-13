
module LSC.SuffixTree where

import Data.Foldable as Fold
import Control.Monad.ST
import Data.Ord
import Data.List (tails)
import Data.IntMap (IntMap)
import qualified Data.IntMap as Map
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Algorithms.Intro as Intro
import qualified Data.Vector.Algorithms.Radix as Radix


newtype SuffixTree a = SuffixTree (IntMap a, SuffixArray, LCP)
  deriving Show

newtype SuffixArray = SuffixArray (Vector Suffix)
  deriving Show

newtype LCP = LCP (Vector Length)
  deriving Show

type Position = Int
type Suffix = (Position, Vector Int)

type Length = Int


constructSuffixTree :: Foldable f => (a -> Int) -> f a -> SuffixTree a
constructSuffixTree goedel xs = SuffixTree (dictionary, suffixArray, lcp)
  where
    lcp = undefined
    dictionary = Map.fromList [(goedel x, x) | x <- Fold.toList xs]
    suffixArray = SuffixArray $ sort $ Vector.fromList
      [ (i, Vector.fromList ys)
      | (i, ys) <- zip [0..] $ tails $ fmap goedel $ Fold.toList xs
      ]


sort :: Vector Suffix -> Vector Suffix
sort v = runST $ do
  m <- Vector.thaw v
  Intro.sort m
  Vector.freeze m

