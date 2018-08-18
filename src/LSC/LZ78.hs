{-# LANGUAGE TypeFamilies #-}

module LSC.LZ78 where

import Data.Generator
import Data.Hashable
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Sequence as Seq
import Data.Sequence ((|>))
import Data.Semigroup.Reducer

data Token a = Token {-# UNPACK #-} !Int Int a
  deriving Show

instance Eq (Token a) where
  Token _ x _ == Token _ y _ = x == y

instance Ord (Token a) where
  Token _ x _ `compare` Token _ y _ = x `compare` y

instance Hashable (Token a) where
  hashWithSalt s (Token i x _) = hashWithSalt s i `hashWithSalt` x

data LZ78 a
  = Cons {-# UNPACK #-} !(Token a) (LZ78 a)
  | Nil
  deriving Show

instance Generator (LZ78 a) where
  type Elem (LZ78 a) = a
  mapTo = go (Seq.singleton mempty) where
    go _ _ m Nil = m
    go s f m (Cons (Token w _ c) ws) = m `mappend` go (s |> v) f v ws where
      v = Seq.index s w `mappend` unit (f c)

encode :: (a -> Int) -> [a] -> LZ78 a
encode goedel = go HashMap.empty 1 0 where
  go _ _ _ [ ] = Nil
  go _ _ p [c] = Cons (Token p (goedel c) c) Nil
  go d f p (c:cs) = let t = Token p (goedel c) c in case HashMap.lookup t d of
    Just p' -> go d f p' cs
    Nothing -> Cons t (go (HashMap.insert t f d) (succ f) 0 cs)



