-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}

module LSC.Transformer where

import Control.Applicative
import Control.Lens
import Control.Exception
import Control.Monad.ST
import Data.Copointed
import Data.Default
import Data.Function
import Data.String

import Control.Monad.IO.Class
import Control.Monad.Codensity
import Control.Monad.Morph
#if MIN_VERSION_base(4,10,0)
import Control.Monad hiding (fail)
import Control.Monad.Fail
import Control.Monad.Reader hiding (fail)
import Control.Monad.State hiding (fail)
#else
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
#endif

import Prelude hiding (fail)

import LSC.Logger
import LSC.Model
import LSC.Trace



type BootstrapT = StateT Technology

type Bootstrap = BootstrapT Identity

bootstrap :: Monad m => (Technology -> Technology) -> BootstrapT m ()
bootstrap = modify

snapshot :: Monad m => BootstrapT m Technology
snapshot = get

frozen :: Monad m => Iso' (BootstrapT m ()) (m Technology)
frozen = iso (flip execStateT def) (put <=< lift)

freeze :: Bootstrap () -> Technology
freeze = copoint . view frozen


type GnosticT = ReaderT Technology

type Gnostic = GnosticT Identity

runGnosticT :: Technology -> GnosticT m r -> m r
runGnosticT = flip runReaderT

runGnostic :: Technology -> Gnostic r -> r
runGnostic x = copoint . runGnosticT x



type ConfinementT = StateT CompilerOpts

type Confinement = ConfinementT Identity


type EnvironmentT = ReaderT CompilerOpts

type Environment = EnvironmentT Identity

runEnvironmentT :: Monad m => CompilerOpts -> EnvironmentT m r -> m r
runEnvironmentT = flip runReaderT

runEnvironment :: CompilerOpts -> Environment r -> r
runEnvironment x = copoint . runEnvironmentT x



type LSC = Codensity (LST IO)

runLSC :: CompilerOpts -> Technology -> LSC a -> IO a
runLSC opts tech
  = runGnosticT tech
  . runEnvironmentT opts
  . unLST
  . lowerCodensity


evalLSC :: CompilerOpts -> Technology -> LSC a -> IO a
evalLSC = runLSC


-- | Once a state thread computation is lifted it may not escape monadic context anymore.
--
liftST :: ST RealWorld a -> LSC a
liftST = liftIO . stToIO



technology :: LSC Technology
technology = lift $ LST $ lift ask

environment :: LSC CompilerOpts
environment = lift $ LST ask


confine :: Confinement () -> LSC a -> LSC a
confine f = lift . LST . local (execState f) . unLST . lowerCodensity



newtype LST m a = LST { unLST :: EnvironmentT (GnosticT m) a }
  deriving Functor 


instance Applicative m => Applicative (LST m) where
  pure = LST . pure
  LST x <*> LST y = LST (x <*> y)

instance Monad m => Monad (LST m) where
  return = LST . return
  m >>= k = LST (unLST m >>= unLST . k)


instance MFunctor LST where
  hoist m = LST . hoist (hoist m) . unLST

instance MonadTrans LST where
  lift = LST . lift . lift


instance MonadIO m => MonadIO (LST m) where
  liftIO = LST . liftIO

#if MIN_VERSION_base(4,10,0)
instance MonadIO m => MonadFail (LST m) where
  fail = catchFail . Fail

newtype Fail = Fail { unFail :: String }
  deriving Exception

instance Show Fail where
  show = unFail


catchFail :: MonadIO m => Fail -> LST m a
catchFail = liftIO . throwIO

assume :: MonadFail m => String -> Bool -> m ()
assume = flip unless . fail
#else
assume :: Monad m => String -> Bool -> m ()
assume = flip unless . fail
#endif


#ifdef DEBUG
instance (Show a, MonadIO m) => Trace (LST m) a where
  trace = liftIO . trace
  {-# INLINE trace #-}

instance Show a => Trace LSC a where
  trace = lift . trace
  {-# INLINE trace #-}
#else
instance (Show a, MonadIO m) => Trace (LST m) a where
  trace = pure
  {-# INLINE trace #-}

instance Show a => Trace LSC a where
  trace = pure
  {-# INLINE trace #-}
#endif


rescale :: Double -> Bootstrap ()
rescale m = do
    k <- view scaleFactor <$> snapshot
    scaleFactor .= m
    let f = round . (/ k) . (* m) . fromIntegral
    stdCells %= (fmap . over vdd . over geometry . fmap) (bimap f f)
    stdCells %= (fmap . over gnd . over geometry . fmap) (bimap f f)
    stdCells %= (fmap . over dims) (bimap f f)
    stdCells %= (fmap . over pins . fmap . over geometry . fmap) (bimap f f)



debug, info, warning :: [String] -> LSC ()
debug   = logger Debug
info    = logger Info
warning = logger Warning


logger :: LogLevel -> [String] -> LSC ()
logger k xs = do
    level <- view logLevel <$> environment
    when (k <= level) $ liftIO $ logStderr k xs


