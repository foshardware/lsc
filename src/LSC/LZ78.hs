{-# LANGUAGE TypeFamilies, ParallelListComp #-}

module LSC.LZ78 where

import Data.Generator
import Data.Hashable
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Sequence as Seq
import Data.Sequence ((|>))
import Data.Semigroup.Reducer


data Token a = Token
  { tokenPos   :: {-# UNPACK #-} !Int
  , tokenIndex :: {-# UNPACK #-} !Int
  , tokenElem  :: a
  , tokenMap   :: a -> Int
  }

instance Show (Token a) where
  show (Token p x a f) = show (p, x, f a)

instance Eq (Token a) where
  Token _ i x f == Token _ j y g = (i, f x) == (j, g y)

instance Ord (Token a) where
  Token _ i x f `compare` Token _ j y g = compare (i, f x) (j, g y)

instance Hashable (Token a) where
  hashWithSalt s (Token _ i a f) = s `hashWithSalt` i `hashWithSalt` f a


data LZ78 a
  = Cons {-# UNPACK #-} !(Token a) (LZ78 a)
  | Nil

instance Generator (LZ78 a) where
  type Elem (LZ78 a) = a
  mapTo = go (Seq.singleton mempty) where
    go _ _ m Nil = m
    go s f m (Cons (Token _ w c _) ws) = m `mappend` go (s |> v) f v ws where
      v = Seq.index s w `mappend` unit (f c)

encode :: (a -> Int) -> [a] -> LZ78 a
encode goedel = go HashMap.empty 0 1 0 where
  go _ _ _ _ [ ] = Nil
  go _ i _ p [c] = Cons (Token i p c goedel) Nil
  go d i f p (c:cs) = let t = Token i p c goedel in case HashMap.lookup t d of
    Just p' -> go d (succ i) f p' cs
    Nothing -> Cons t (go (HashMap.insert t f d) (succ i) (succ f) 0 cs)

entries :: LZ78 a -> [Token a]
entries Nil = []
entries (Cons w ws) = w : entries ws

poi :: Int -> LZ78 a -> [Token a]
poi k ws
  =
  [ u
  | (t, u) <- entries ws `zip` drop 1 (entries ws)
  , tokenPos u - tokenPos t > k
  ]

instance Show (LZ78 a) where
  show lz = "[" ++ go lz where
    go (Cons t Nil) = show t ++ "]"
    go (Cons t r) = show t ++ "," ++ go r
    go Nil = ""


