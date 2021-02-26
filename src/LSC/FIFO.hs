-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}

module LSC.FIFO
    ( FIFO
    , enqueue, dequeue, deq
    , fromList
    ) where

#if !MIN_VERSION_base(4,10,0)
import Data.Semigroup
#endif

import Control.Applicative
import Data.Foldable


data FIFO a = FIFO
  {-# UNPACK #-} !Int
  {-# UNPACK #-} !Int
  [a] [a]
  deriving Functor


fifo :: Int -> Int -> [a] -> [a] -> FIFO a
fifo lf lr fs rs
    | lf < lr
    = FIFO (lf + lr) 0 (fs ++ reverse rs) []
fifo lf lr fs rs
    = FIFO lf lr fs rs


enqueue :: a -> FIFO a -> FIFO a
enqueue x (FIFO lf lr fs rs) = fifo lf (succ lr) fs (x : rs)


dequeue :: FIFO a -> (Maybe a, FIFO a)
dequeue (FIFO lf lr (x : fs) rs) = (Just x, fifo (pred lf) lr fs rs)
dequeue _ = (Nothing, empty)


deq :: FIFO a -> (a, FIFO a)
deq (FIFO lf lr (x : fs) rs) = (x, fifo (pred lf) lr fs rs)
deq _ = error "deq: empty queue"


fromList :: [a] -> FIFO a
fromList xs = FIFO (length xs) 0 xs []



instance Applicative FIFO where
    pure x = FIFO 1 0 [x] []
    FIFO lf lr fs rs <*> FIFO lg ls gs ss
        = FIFO ((lf + lr) * (lg + ls)) 0 (fs ++ reverse rs <*> gs ++ reverse ss) []


instance Alternative FIFO where
    empty = FIFO 0 0 [] []
    FIFO lf lr fs rs <|> FIFO lg ls gs ss
        = FIFO (lf + lr + lg + ls) 0 (fs ++ reverse rs ++ gs ++ reverse ss) []


instance Foldable FIFO where

    foldr g s (FIFO _ _ fs rs) = foldr g s (fs ++ reverse rs)
    foldl g s (FIFO _ _ fs rs) = foldl g s (fs ++ reverse rs)

#if MIN_VERSION_base(4,10,0)
    foldMap g (FIFO _ _ fs rs) = foldMap g fs <> foldMap g (reverse rs)
#else
    foldMap g (FIFO _ _ fs rs) = foldMap g fs `mappend` foldMap g (reverse rs)
#endif

    foldl' g s (FIFO _ _ fs rs) = foldl' g s (fs ++ reverse rs)
#if MIN_VERSION_base(4,13,0)
    foldMap' g (FIFO _ _ fs rs) = case foldMap' g fs <> foldMap' g (reverse rs) of m -> m
#endif

    toList (FIFO _ _ fs rs) = fs ++ reverse rs

    length (FIFO lf lr _ _) = lf + lr

    elem i (FIFO _ _ fs rs) = elem i fs || elem i rs

    maximum (FIFO _ _ fs rs) = maximum (fs ++ rs)
    minimum (FIFO _ _ fs rs) = minimum (fs ++ rs)

    sum (FIFO _ _ fs rs) = sum fs + sum rs
    product (FIFO _ _ fs rs) = product fs * product rs


instance Traversable FIFO where
    traverse f (FIFO lf lr fs rs)
        = FIFO (lf + lr) 0 <$> ((++) <$> traverse f fs <*> traverse f (reverse rs)) <*> pure []


instance Semigroup (FIFO a) where
    (<>) = (<|>)


instance Monoid (FIFO a) where
    mempty = empty
#if !MIN_VERSION_base(4,11,0)
    mappend = (<>)
#endif

