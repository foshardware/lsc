
module LSC.SuffixTree where

import qualified Data.Foldable as Fold
import Control.Monad
import Control.Monad.ST
import Data.Foldable (for_)
import Data.Function (on)
import Data.List (tails)
import Data.STRef
import qualified Data.IntSet as Set
import Data.Vector hiding (zip, minimum, init)
import Data.Vector.Mutable (new, write)
import qualified Data.Vector.Unboxed as Unboxed
import qualified Data.Vector.Algorithms.Intro as Intro
import qualified Data.Vector.Algorithms.Radix as Radix
import Prelude hiding (length, (++), take, drop, reverse, filter)
import Debug.Trace


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

    string = fromList $ Fold.toList xs

    suffixArray = sortBy (compare `on` snd) $ fromList
      [ (i, Unboxed.fromList ys)
      | (i, ys) <- zip [1..] $ tails [goedel x | x <- toList string]
      ]

    lcp = fromList
      [ (i, Unboxed.foldr commonPrefix 0 $ Unboxed.zip x y)
      | k <- [0 .. length suffixArray - 1]
      , let (i, x) = suffixArray ! k
      , let (_, y) = if k > 0 then suffixArray ! (k-1) else (undefined, mempty)
      ]


findmaxr :: Foldable f => (a -> Int) -> f a -> Int -> [Vector a]
findmaxr goedel xs ml = runST $ do

  let SuffixTree w r lcp = constructSuffixTree goedel xs

  result <- newSTRef []

  let n = length w
  let p = reverse r

  _S <- newSTRef $ Set.fromList $ 0: n: [ u | u <- [1 .. n - 1], snd (lcp! u) < ml ]

  let _I = sortBy ( \ i j -> snd (lcp! i) `compare` snd (lcp! j)) $ fromList [0 .. n-1]

  let initial = minimum $ n: [ t | t <- toList _I, snd (lcp! (_I! t)) >= ml ]

  for_ [ initial .. n - 1] $ \ t -> do

    let i = _I! t

    pi <- maybe i succ . Set.lookupLT i <$> readSTRef _S
    ni <- maybe i   id . Set.lookupGT i <$> readSTRef _S

    modifySTRef _S $ Set.insert i

    if (pi == 1 || snd (lcp! (pi-1)) /= snd (lcp! i))
      && (ni == n || snd (lcp! ni) /= snd (lcp! i))
    then do
      if fst (r! pi) == 1 || fst (r! ni) == 1
        || goedel (w! (fst (r! pi) - 1)) /= goedel (w! (fst (r! ni) - 1))
        || fst (p! (fst (r! ni) - 1)) - fst (p! (fst (r! pi) - 1)) /= ni - pi
      then do

        modifySTRef result (slice (fst (r! i) - 1) (snd (lcp! i)) w :)

      else pure ()

    else pure ()

  readSTRef result


commonPrefix :: (Int, Int) -> Int -> Int
commonPrefix (a, b) s | a == b = 1 + s
commonPrefix _ _ = 0


longestSubString :: SuffixTree a -> (Int, Int)
longestSubString (SuffixTree _ _ lcp)
  = maximumBy ( \ a b -> snd a `compare` snd b ) lcp


sortBy :: (a -> a -> Ordering) -> Vector a -> Vector a
sortBy f v = runST $ do
  m <- thaw v
  Intro.sortBy f m
  unsafeFreeze m 

