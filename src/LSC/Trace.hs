-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module LSC.Trace where

import Control.Monad.ST
#ifdef DEBUG
import Control.Applicative
import Control.Monad.ST.Unsafe
import System.IO.Unsafe
import System.IO
#endif


class Trace m a where
    trace :: a -> m a
    -- pointfree usage for `trace :: a -> b -> a`
    -- - `liftA2 trace id id`
    -- - `flip trace ()`
    --


#ifdef DEBUG
instance Show a => Trace IO a where
    trace = liftA2 (<$) id (hPutStrLn stderr . show)
    {-# NOINLINE trace #-}

instance Show a => Trace (ST s) a where
    trace = unsafeIOToST . trace
    {-# NOINLINE trace #-}

instance Show a => Trace ((->) b) a where
    trace = const . unsafePerformIO . trace
    {-# NOINLINE trace #-}
#else
instance Show a => Trace IO a where
    trace = pure
    {-# INLINE trace #-}

instance Show a => Trace (ST s) a where
    trace = pure
    {-# INLINE trace #-}

instance Show a => Trace ((->) b) a where
    trace = const
    {-# INLINE trace #-}
#endif

