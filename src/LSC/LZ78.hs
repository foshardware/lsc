{-# LANGUAGE TypeFamilies #-}

module LSC.LZ78 where

import Data.Generator
import Data.Hashable
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Sequence as Seq
import Data.Sequence ((|>))
import Data.Semigroup.Reducer


data Token a = Token {-# UNPACK #-} !Int a (a -> Int)

instance Show (Token a) where
  show (Token x a f) = show (x, f a)

instance Eq (Token a) where
  Token i x f == Token j y g = (i, f x) == (j, g y)

instance Ord (Token a) where
  Token i x f `compare` Token j y g = compare (i, f x) (j, g y)

instance Hashable (Token a) where
  hashWithSalt s (Token i a f) = s `hashWithSalt` i `hashWithSalt` f a


data LZ78 a
  = Cons {-# UNPACK #-} !(Token a) (LZ78 a)
  | Nil

instance Generator (LZ78 a) where
  type Elem (LZ78 a) = a
  mapTo = go (Seq.singleton mempty) where
    go _ _ m Nil = m
    go s f m (Cons (Token w c _) ws) = m `mappend` go (s |> v) f v ws where
      v = Seq.index s w `mappend` unit (f c)

encode :: (a -> Int) -> [a] -> LZ78 a
encode goedel = go HashMap.empty 1 0 where
  go _ _ _ [ ] = Nil
  go _ _ p [c] = Cons (Token p c goedel) Nil
  go d f p (c:cs) = let t = Token p c goedel in case HashMap.lookup t d of
    Just p' -> go d f p' cs
    Nothing -> Cons t (go (HashMap.insert t f d) (succ f) 0 cs)

entries :: LZ78 a -> [Int]
entries Nil = []
entries (Cons (Token w _ _) ws) = w : entries ws

instance Show (LZ78 a) where
  show lz = "[" ++ go lz where
    go (Cons t Nil) = show t ++ "]"
    go (Cons t r) = show t ++ "," ++ go r
    go Nil = ""


