-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module LSC.Trace where

import Control.Monad.ST
import Control.Monad.State as Lazy
import Control.Monad.State.Strict as Strict
import Data.Functor.Identity
#ifdef DEBUG
import Control.Monad.ST.Unsafe
import System.IO.Unsafe
import System.IO
#endif


class Monad m => Trace a m where

    trace :: a -> m a
    trace = pure
    -- pointfree usage of `trace :: a -> a -> a`:
    -- - `trace <$> id <*> id`
    --


#ifdef DEBUG

instance Show a => Trace a ((->) b) where
    trace = const . unsafePerformIO . trace
    {-# NOINLINE trace #-}

instance Show a => Trace a Identity where
    trace m = pure $ trace m m
    {-# NOINLINE trace #-}

instance Show a => Trace a IO where
    trace m = m <$ hPutStrLn stderr (show m)
    {-# NOINLINE trace #-}

instance Show a => Trace a (ST s) where
    trace = unsafeIOToST . trace
    {-# NOINLINE trace #-}

instance Trace a m => Trace a (Lazy.StateT s m) where
    trace = lift . trace
    {-# NOINLINE trace #-}

instance Trace a m => Trace a (Strict.StateT s m) where
    trace = lift . trace
    {-# NOINLINE trace #-}

#else

instance Show a => Trace a ((->) b)

instance Show a => Trace a Identity

instance Show a => Trace a IO

instance Show a => Trace a (ST s)

instance Trace a m => Trace a (Lazy.StateT s m)

instance Trace a m => Trace a (Strict.StateT s m)

#endif

