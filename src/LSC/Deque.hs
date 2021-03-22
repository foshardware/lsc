-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE DeriveFunctor #-}

module LSC.Deque
  ( Deque
  , enqueue, dequeue, deq
  , fromList
  ) where

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

    foldMap g (Deque _ _ fs rs) = foldMap g fs <> foldMap g (reverse rs)

    foldl' g s (Deque _ _ fs rs) = foldl' g s (fs ++ reverse rs)

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

