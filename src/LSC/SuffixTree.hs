
module LSC.SuffixTree where

import Data.Foldable as Fold
import Control.Monad.ST
import Data.Ord
import Data.List (tails)
import Data.IntMap (IntMap)
import qualified Data.IntMap as Map
import Data.Vector (Vector, (!))
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
constructSuffixTree goedel xs = SuffixTree (dictionary, SuffixArray suffixVector, LCP lcp)
  where
    dictionary = Map.fromList [(goedel x, x) | x <- Fold.toList xs]
    suffixVector = radixSort $ Vector.fromList
      [ (i, Vector.fromList ys)
      | (i, ys) <- zip [0..] $ tails $ [goedel x | x <- Fold.toList xs]
      ]
    lcp = Vector.fromList $ 0 : 
      [ commonPrefixLength (suffixVector ! i) (suffixVector ! (i - 1))
      | i <- [1 .. Vector.length suffixVector - 1]
      ] 


commonPrefixLength :: Suffix -> Suffix -> Int
commonPrefixLength (_, xs) (_, ys)
  | Vector.null xs || Vector.null ys
  = 0
commonPrefixLength (p, xs_) (q, ys_)
  | (x, xs) <- Vector.splitAt 1 xs_
  , (y, ys) <- Vector.splitAt 1 ys_
  , x == y
  = 1 + commonPrefixLength (p, xs) (q, ys)
commonPrefixLength _ _ = 0


radixSort :: Vector Suffix -> Vector Suffix
radixSort v = runST $ do
  m <- Vector.thaw v
  Intro.sortBy comp m
  Vector.freeze m
  where comp (_, a) (_, b) = compare a b

