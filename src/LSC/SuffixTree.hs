
module LSC.SuffixTree where

import Data.Foldable as Fold
import Control.Monad.ST
import Data.List (tails)
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vector
import qualified Data.Vector.Unboxed as Unboxed
import qualified Data.Vector.Algorithms.Intro as Intro
import qualified Data.Vector.Algorithms.Radix as Radix


data SuffixTree a = SuffixTree (Vector a) SuffixArray LCP
  deriving Show

type SuffixArray = Vector (Position, Suffix)

type LCP = Vector (Position, Length)

type Position = Int
type Suffix = Unboxed.Vector Int

type Length = Int


constructSuffixTree :: Foldable f => (a -> Int) -> f a -> SuffixTree a
constructSuffixTree goedel xs = SuffixTree string suffixArray lcp
  
  where

    string = Vector.fromList $ Fold.toList xs

    suffixArray = sortBy snd $ Vector.fromList
      [ (i, Unboxed.fromList ys)
      | (i, ys) <- zip [0..] $ tails [goedel x | x <- Vector.toList string]
      ]

    lcp = Vector.fromList
      [ (i, Unboxed.foldr commonPrefix 0 $ Unboxed.zip x y)
      | k <- [1 .. Vector.length suffixArray - 1]
      , let (i, x) = suffixArray ! k
      , let (_, y) = suffixArray ! (k-1)
      ]


commonPrefix :: (Int, Int) -> Int -> Int
commonPrefix (a, b) s | a == b = 1 + s
commonPrefix _ _ = 0


longestSubstring :: SuffixTree a -> (Int, Int, Vector a)
longestSubstring (SuffixTree string _ lcp)
  = (ix, len, Vector.take len $ Vector.drop ix $ string)
  where (ix, len) = maximumBy ( \ a b -> snd a `compare` snd b ) lcp


sortBy :: (a -> Suffix) -> Vector a -> Vector a
sortBy f v = runST $ do
  m <- Vector.thaw v
  Intro.sortBy ( \ a b -> f a `compare` f b ) m
  Vector.freeze m

