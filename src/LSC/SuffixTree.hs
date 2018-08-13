
module LSC.SuffixTree where

import Data.Ord
import Data.Functor.Identity
import Data.Vector hiding (zip)


newtype SuffixTreeT m a = SuffixTreeT (PredicateT m a, SuffixArray a, LCP a)

newtype SuffixArray a = SuffixArray (Vector (Position, Suffix a))

newtype LCP a = LCP (Vector Length)

type PredicateT m a = a -> a -> m Ordering
type Predicate = PredicateT Identity

type Position = Int
type Suffix a = [a]

type Length = Int


type SuffixTree a = SuffixTreeT Identity a

constructSuffixTree :: Predicate a -> [a] -> SuffixTree a
constructSuffixTree predicate xs = SuffixTreeT (proposition, suffixArray, lcp)
  where
    lcp = undefined
    suffixArray = SuffixArray $ fromList [(i, ys) | (i, ys) <- zip [0..] $ tails xs]
    proposition a b = pure $ predicate a b


