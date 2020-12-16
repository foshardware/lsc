{-# LANGUAGE BangPatterns #-}

module LSC.FIFO
    ( FIFO
    , enqueue, dequeue, deq
    , fromList
    ) where

import Control.Applicative
import Data.Foldable


data FIFO a = FIFO !Int !Int [a] [a]

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



instance Functor FIFO where
    fmap g (FIFO lf lr fs rs) = FIFO lf lr (fmap g fs) (fmap g rs)


instance Applicative FIFO where
    pure x = FIFO 1 0 [x] []
    FIFO lg ls gs ss <*> FIFO lf lr fs rs
        = FIFO
        (lf * (lg + ls))
        (lr * (lg + ls))
        (foldMap (<$> fs) (gs ++ reverse ss))
        (foldMap (<$> rs) (ss ++ reverse gs))


instance Alternative FIFO where
    empty = FIFO 0 0 [] []
    FIFO lf lr fs rs <|> FIFO lg ls gs ss
        = fifo lf (lr + lg + ls) fs (rs ++ reverse gs ++ ss)


instance Monad FIFO where
    return = pure
    FIFO _ _ fs rs >>= k
        = fifo
        (sum $ length <$> gs)
        (sum $ length <$> ss)
        (toList =<< gs)
        (reverse . toList =<< ss)
        where gs = k <$> fs
              ss = k <$> rs



instance Foldable FIFO where

    foldr g s (FIFO _ _ fs rs) = foldr g s (fs ++ reverse rs)
    foldl g s (FIFO _ _ fs rs) = foldl g s (fs ++ reverse rs)

    foldMap g (FIFO _ _ fs rs) = foldMap g fs <> foldMap g (reverse rs)

    foldl' g s (FIFO _ _ fs rs) = foldl' g s (fs ++ reverse rs)
    foldMap' g (FIFO _ _ fs rs) = let !m = foldMap' g fs <> foldMap' g (reverse rs) in m

    toList (FIFO _ _ fs rs) = fs ++ reverse rs

    length (FIFO lf lr _ _) = lf + lr

    elem i (FIFO _ _ fs rs) = elem i fs || elem i rs

    maximum (FIFO _ _ fs rs) = maximum (fs ++ rs)
    minimum (FIFO _ _ fs rs) = minimum (fs ++ rs)

    sum (FIFO _ _ fs rs) = sum fs + sum rs
    product (FIFO _ _ fs rs) = product fs * product rs



instance Semigroup (FIFO a) where
    (<>) = (<|>)


instance Monoid (FIFO a) where
    mempty = empty
    mappend = (<>)


