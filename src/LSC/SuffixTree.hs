
module LSC.SuffixTree where

import qualified Data.Foldable as Fold
import Control.Monad.ST
import Data.Foldable hiding (reverse)
import Data.Function (on)
import Data.List (tails)
import Data.STRef
import qualified Data.IntSet as Set
import Data.Vector
  ( Vector
  , unsafeFreeze, freeze, thaw
  , generate, (!)
  , reverse, drop
  , fromList
  )
import qualified Data.Vector.Unboxed as Unboxed
import qualified Data.Vector.Algorithms.Intro as Intro
import qualified Data.Vector.Algorithms.Radix as Radix
import Prelude hiding (reverse, drop)


data SuffixTree a = SuffixTree (Vector a) SuffixArray LCP
  deriving Show

type SuffixArray = Vector (Position, Suffix)

type LCP = Vector (Position, Length)

type Position = Int
type Suffix = Unboxed.Vector Int

type Length = Int


constructSuffixTree :: (a -> Int) -> Vector a -> SuffixTree a
constructSuffixTree goedel string = SuffixTree string suffixArray lcp
  
  where

    suffixArray
      = sortBy (compare `on` snd)
      $ generate (length string + 1)
      $ \ k -> (k + 1, Unboxed.generate (length string - k) (drop k (goedel <$> string) !))

    lcp = generate (length suffixArray) gen where
      gen k = (i, Unboxed.foldr commonPrefix 0 $ Unboxed.zip x y) where
        (i, x) = suffixArray ! k
        (_, y) = case k of
            0 -> (undefined, mempty)
            _ -> suffixArray ! (k-1)


maximalRepeatsDisjoint
  :: Foldable f
  => (a -> Int)
  -> f a
  -> Int
  -> Vector (Length, [Position], Int)
maximalRepeatsDisjoint goedel xs ml
  = sortBy ( \ (_, _, x) (_, _, y) -> compare y x)
  $ fromList

  [ (len, xs, len * length xs)
  | (len, rs) <- runST $ findmaxr goedel string ml
  , let xs = foldr (disjoint len) [] rs
  ]

  where

    string = fromList $ Fold.toList xs

    disjoint l p (x : xs) | x + l > p = x : xs
    disjoint _ p xs = p : xs


findmaxr :: (a -> Int) -> Vector a -> Int -> ST s [(Length, [Position])]
findmaxr goedel string ml = do

  let SuffixTree w r lcp = constructSuffixTree goedel string

  result <- newSTRef []

  let n = length w
  let p = reverse r

  -- discard all positions where length of least common prefix < ml
  _S <- newSTRef $ Set.fromList [ u | u <- [1 .. n-1], snd (lcp! u) < ml ]

  let _I = sortBy (compare `on` snd . (lcp!)) $ generate n id

  let initial = minimum $ n: [ t | t <- toList _I, snd (lcp! (_I! t)) >= ml ]

  for_ [ initial .. n-1 ] $ \ t -> do

    let i = _I! t

    p' <- maybe 0 succ . Set.lookupLT i <$> readSTRef _S
    n' <- maybe n   id . Set.lookupGT i <$> readSTRef _S

    modifySTRef _S $ Set.insert i

    -- check if this substring is maximal to the right
    if   (p' == 1 || snd (lcp! (p'-1)) /= snd (lcp! i))
      && (n' == n || snd (lcp!     n') /= snd (lcp! i))

    then do

      -- check if this substring is maximal to the left
      if   fst (r! p') == 1 || fst (r! n') == 1
        || goedel (w! (fst (r! p') - 1)) /= goedel (w! (fst (r! n') - 1))
        || fst (p! (fst (r! n') - 1)) - fst (p! (fst (r! p') - 1)) /= n' - p'

      then do

        -- multiple overlapping occurences
        let pos = [ fst (r! k) - 1 | k <- [p' - 1 .. n' - 1] ]
            len = snd (lcp! i)

        modifySTRef result ((len, pos) :)

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

