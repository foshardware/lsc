-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}

-- | Assorted higher-order functions
--
module LSC.HigherOrder where

import Data.Foldable


ifoldl' :: Foldable f => (Int -> b -> a -> b) -> b -> f a -> b
ifoldl' f y xs = foldl' (\ g x !i -> f i (g (i - 1)) x) (const y) xs (length xs - 1)
{-# INLINE ifoldl' #-}


#if !MIN_VERSION_base(4,13,0)

foldMap' :: (Foldable f, Monoid m) => (a -> m) -> f a -> m
foldMap' f = foldl' (\ acc a -> acc `mappend` f a) mempty
{-# INLINE foldMap' #-}

#endif

