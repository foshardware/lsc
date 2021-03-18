-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}

module LSC.Deque
  ( Deque
  , enqueue, dequeue, deq
  , fromList
  ) where

#if !MIN_VERSION_base(4,10,0)
import Data.Semigroup
#endif

import Control.Applicative
import Data.Foldable


data Deque a = Deque
  {-# UNPACK #-} !Int
  {-# UNPACK #-} !Int
  [a] [a]
  deriving Functor


deque :: Int -> Int -> [a] -> [a] -> Deque a
deque lf lr fs rs
  | lf < lr
  = Deque (lf + lr) 0 (fs ++ reverse rs) []
deque lf lr fs rs
  = Deque lf lr fs rs


enqueue :: a -> Deque a -> Deque a
enqueue x (Deque lf lr fs rs) = deque lf (succ lr) fs (x : rs)


dequeue :: Deque a -> (Maybe a, Deque a)
dequeue (Deque lf lr (x : fs) rs) = (Just x, deque (pred lf) lr fs rs)
dequeue _ = (Nothing, empty)


deq :: Deque a -> (a, Deque a)
deq (Deque lf lr (x : fs) rs) = (x, deque (pred lf) lr fs rs)
deq _ = error "deq: empty queue"


fromList :: Foldable f => f a -> Deque a
fromList xs = Deque (length xs) 0 (toList xs) []
{-# INLINABLE fromList #-}


instance Applicative Deque where
    pure x = Deque 1 0 [x] []
    Deque lf lr fs rs <*> Deque lg ls gs ss
      = Deque ((lf + lr) * (lg + ls)) 0 ((fs ++ reverse rs) <*> (gs ++ reverse ss)) []


instance Alternative Deque where
    empty = Deque 0 0 [] []
    Deque lf lr fs rs <|> Deque lg ls gs ss
      = Deque (lf + lr + lg + ls) 0 (fs ++ reverse rs ++ gs ++ reverse ss) []


instance Foldable Deque where

    foldr g s (Deque _ _ fs rs) = foldr g s (fs ++ reverse rs)
    foldl g s (Deque _ _ fs rs) = foldl g s (fs ++ reverse rs)

#if MIN_VERSION_base(4,10,0)
    foldMap g (Deque _ _ fs rs) = foldMap g fs <> foldMap g (reverse rs)
#else
    foldMap g (Deque _ _ fs rs) = foldMap g fs `mappend` foldMap g (reverse rs)
#endif

    foldl' g s (Deque _ _ fs rs) = foldl' g s (fs ++ reverse rs)
#if MIN_VERSION_base(4,13,0)
    foldMap' g (Deque _ _ fs rs) = case foldMap' g fs <> foldMap' g (reverse rs) of m -> m
#endif

    toList (Deque _ _ fs rs) = fs ++ reverse rs

    length (Deque lf lr _ _) = lf + lr

    elem i (Deque _ _ fs rs) = elem i fs || elem i rs

    maximum (Deque _ _ fs rs) = maximum (fs ++ rs)
    minimum (Deque _ _ fs rs) = minimum (fs ++ rs)

    sum (Deque _ _ fs rs) = sum fs + sum rs
    product (Deque _ _ fs rs) = product fs * product rs


instance Traversable Deque where
    traverse f (Deque lf lr fs rs)
      = Deque (lf + lr) 0 <$> ((++) <$> traverse f fs <*> traverse f (reverse rs)) <*> pure []


instance Semigroup (Deque a) where
    (<>) = (<|>)


instance Monoid (Deque a) where
    mempty = empty
#if !MIN_VERSION_base(4,11,0)
    mappend = (<>)
#endif

