-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}

module LSC.Trace where

import Control.Monad.ST
import Control.Monad.State as Lazy
import Control.Monad.State.Strict as Strict
#ifdef DEBUG
import Control.Monad.ST.Unsafe
import System.IO.Unsafe
import System.IO
#endif


class Trace m a where
    trace :: a -> m a
    -- pointfree usage of `trace :: a -> a -> a`:
    -- - `liftA2 trace id id`
    -- - `trace undefined`
    -- - `trace mempty`
    --


#ifdef DEBUG
instance Show a => Trace IO a where
    trace m = m <$ hPutStrLn stderr (show m)
    {-# NOINLINE trace #-}

instance Show a => Trace (ST s) a where
    trace = unsafeIOToST . trace
    {-# NOINLINE trace #-}

instance (Monad m, Show a) => Trace (Lazy.StateT s m) a where
    trace m = trace m m `seq` pure m
    {-# NOINLINE trace #-}

instance (Monad m, Show a) => Trace (Strict.StateT s m) a where
    trace m = trace m m `seq` pure m
    {-# NOINLINE trace #-}

instance Show a => Trace ((->) a) a where
    trace = const $ unsafePerformIO . trace
    {-# NOINLINE trace #-}
#else
instance Show a => Trace IO a where
    trace = pure
    {-# INLINE trace #-}

instance Show a => Trace (ST s) a where
    trace = pure
    {-# INLINE trace #-}

instance (Monad m, Show a) => Trace (Lazy.StateT s m) a where
    trace = pure
    {-# INLINE trace #-}

instance (Monad m, Show a) => Trace (Strict.StateT s m) a where
    trace = pure
    {-# INLINE trace #-}

instance Show a => Trace ((->) a) a where
    trace = const id
    {-# INLINE trace #-}
#endif

