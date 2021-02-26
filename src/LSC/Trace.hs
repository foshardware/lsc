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
    -- recommended usage for `trace :: a -> a -> a` is either:
    -- - liftA2 trace id id
    -- - trace undefined


#ifdef DEBUG
instance Show a => Trace IO a where
    trace = liftA2 (<$) id $ hPutStrLn stderr . show
    {-# INLINE trace #-}

instance Show a => Trace (ST s) a where
    trace = unsafeIOToST . trace
    {-# INLINE trace #-}

instance Show a => Trace ((->) a) a where
    trace = const $ unsafePerformIO . trace
    {-# INLINE trace #-}
#else
instance Show a => Trace IO a where
    trace = pure
    {-# INLINE trace #-}

instance Show a => Trace (ST s) a where
    trace = pure
    {-# INLINE trace #-}

instance Show a => Trace ((->) a) a where
    trace = const id
    {-# INLINE trace #-}
#endif

