-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveTraversable #-}

module LSC.Transformer
  ( BootstrapT, Bootstrap
  , bootstrap, snapshot, rescale
  , frozen, deepFreeze
  , GnosticT, Gnostic
  , runGnosticT, runGnostic

  , ConfinementT, Confinement
  , EnvironmentT, Environment
  , runEnvironmentT, runEnvironment

  , Codensity, codensity
  , Identity, identity

  , Cursor, runCursor, evalCursor, execCursor
  , hover, position, cursor

  , LSC
  , elevate, generalize

  , Fail, IllegalLiftIO, G

  , runLSC, evalLSC
  , context, thread, semaphore
  , indeterminate, entropicIndeterminate
  , determinate, entropicDeterminate
  , technology, environment, confine

  , assume, throwLSC
  , warning, info, debug

  ) where

import Control.Applicative
import Control.Concurrent.MSem (with)
import Control.Lens
import Control.DeepSeq
import Control.Exception
import Data.Bifoldable
import Data.Default
import Data.Scientific
import Data.STRef

import Control.Monad.Primitive
import Control.Monad.ST

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Codensity
import Control.Monad.Except (ExceptT, runExceptT, mapExceptT, throwError)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Fail
import Control.Monad.Morph
import Control.Monad.Reader (ReaderT, runReaderT, local, ask)
import Control.Monad.State (MonadState, StateT, execStateT, execState, put, get, modify)

import GHC.Generics hiding (from, to)
import Prelude hiding (fail)

import LSC.Entropy
import LSC.HigherOrder
import LSC.Log
import LSC.Model
import LSC.Trace




type Cursor s r = ReaderT (STRef s r) (ST s)

runCursor :: Cursor s r a -> r -> ST s (a, r)
runCursor c r = do
  s <- newSTRef r
  (,) <$> runReaderT c s <*> readSTRef s
 
 
evalCursor :: Cursor s r a -> r -> ST s a
evalCursor c r = fst <$> runCursor c r

execCursor :: Cursor s r () -> r -> ST s r
execCursor c r = snd <$> runCursor c r


hover :: (r -> r) -> Cursor s r ()
hover f = cursor . flip modifySTRef f =<< ask

position :: (r -> a) -> Cursor s r a
position g = fmap g . cursor . readSTRef =<< ask

cursor :: ST s a -> Cursor s r a
cursor = lift



type BootstrapT = StateT Technology

type Bootstrap = BootstrapT Identity

bootstrap :: MonadState Technology m => (Technology -> Technology) -> m ()
bootstrap = modify

snapshot :: MonadState Technology m => m Technology
snapshot = get



type GnosticT = ReaderT Technology

type Gnostic = GnosticT Identity

runGnosticT :: Technology -> GnosticT m r -> m r
runGnosticT = flip runReaderT

runGnostic :: Technology -> Gnostic r -> r
runGnostic x = view (from identity) . runGnosticT x



type ConfinementT = StateT CompilerOpts

type Confinement = ConfinementT Identity


type EnvironmentT = ReaderT CompilerOpts

type Environment = EnvironmentT Identity

runEnvironmentT :: Monad m => CompilerOpts -> EnvironmentT m r -> m r
runEnvironmentT = flip runReaderT

runEnvironment :: CompilerOpts -> Environment r -> r
runEnvironment x = view (from identity) . runEnvironmentT x



-- | Note that `LSC` is pure when m is pure.
--
type LSC m = Codensity (EnvironmentT (GnosticT (G m Fail)))


instance Log w m => Log w (LSC m) where
  enter k xs = do
    level <- view logLevel <$> environment
    when (k <= level)
      $ lift . lift . lift . G . lift
      $ enter k xs


instance Trace a m => Trace a (LSC m) where
  trace = lift . lift . lift . G . lift . trace
  {-# INLINE trace #-}



elevate :: Monad m => (forall a. Identity a -> m a) -> LSC Identity b -> LSC m b
elevate f
  = under codensity
  $ hoist
  $ hoist
  $ G . hoist f . unG


runLSC :: Monad m => CompilerOpts -> Technology -> LSC m a -> m (Either Fail a)
runLSC opts tech
  = runExceptT
  . unG
  . runGnosticT tech
  . runEnvironmentT opts
  . lowerCodensity


evalLSC :: MonadIO m => CompilerOpts -> Technology -> LSC m a -> m a
evalLSC opts tech = either (liftIO . throwIO) pure <=< runLSC opts tech


context :: Monad m => LSC m (LSC m a -> m (Either Fail a))
context = runLSC <$> environment <*> technology



indeterminate :: (MonadIO m, PrimBase p, PrimState p ~ RealWorld) => p a -> LSC m a
indeterminate = lift . lift . lift . G . liftIO . primToIO
{-# INLINABLE indeterminate #-}


entropicIndeterminate
  :: (MonadIO m, PrimBase p, PrimState p ~ RealWorld)
  => (Gen (PrimState p) -> p a)
  -> LSC m a
entropicIndeterminate k
  = indeterminate
  . flip nonDeterministically k
  . view entropy
    =<< environment


-- | Can `s` be free for any type other than `ST`?
--
determinate :: (forall s. ST s a) -> LSC m a
determinate m = pure $! runST m

entropicDeterminate :: (forall s. Gen s -> ST s a) -> LSC m a
entropicDeterminate k = determinate $ deterministically k


technology :: Monad m => LSC m Technology
technology = lift $ lift ask

environment :: Monad m => LSC m CompilerOpts
environment = lift ask


confine :: Monad m => Confinement () -> LSC m a -> LSC m a
confine = under codensity . local . execState


throwLSC :: (MonadIO m, Exception e) => e -> LSC m a
throwLSC = indeterminate . throwIO


assume :: MonadFail m => String -> Bool -> m ()
assume = flip unless . fail



newtype Fail = Fail { unFail :: Entry }
  deriving (Exception, Generic, NFData)
  deriving (Semigroup, Monoid) via Entry

instance Show Fail where
  show = unlines . unFail


data IllegalLiftIO = IllegalLiftIO
  deriving Exception

instance Show IllegalLiftIO where
  show = const "illegal liftIO"



-- | Lifting to `G` is forbidden and its data constructor is not exported from this module.
--   rationale: inexpressive, unconstrained effects.
--   forbidden instances: MFunctor, MonadTrans, MonadIO
--
--   Observe that bifunctors are not well-kinded to be monad transformers:
--
--   `G` is of kind
-- > (* -> *) -> * -> * -> *
--
--   Transformers must be of kind
-- > (* -> *) -> * -> *
--
--
newtype G m e a = G { unG :: ExceptT e m a }
  deriving (Functor, Foldable, Traversable)
  deriving (Applicative, Alternative, Monad, MonadFix) via ExceptT e m


instance Functor m => Bifunctor (G m) where
  bimap f g = G . mapExceptT (bimap f g <$>) . unG

instance Foldable m => Bifoldable (G m) where
  bifoldMap f g = foldMap (bifoldMap f g) . runExceptT . unG


instance Monad m => MonadFail (G m Fail) where
  fail = G . throwError . Fail . pure


-- | This will throw an exception in `m`
--
instance MonadIO m => MonadIO (G m e) where
  liftIO _ = G . liftIO $ throwIO IllegalLiftIO



identity :: Iso a b (Identity a) (Identity b)
identity = iso Identity runIdentity


codensity :: (Monad f, Monad g) => Iso (f a) (g b) (Codensity f a) (Codensity g b)
codensity = iso lift lowerCodensity


frozen :: (Monad f, Monad g) => Iso (BootstrapT f ()) (BootstrapT g ()) (f Technology) (g Technology)
frozen = iso (`execStateT` def) (put <=< lift)



rescale :: MonadState Technology m => Scientific -> m ()
rescale m
  = do
    k <- view scaleFactor <$> snapshot
    scaleFactor .= m
    let f = round @Double . (/ toRealFloat k) . (* toRealFloat m) . fromIntegral
    stdCells %= (fmap . over vdd . over geometry . fmap) (bimap f f)
    stdCells %= (fmap . over gnd . over geometry . fmap) (bimap f f)
    stdCells %= (fmap . over dims) (bimap f f)
    stdCells %= (fmap . over pins . fmap . over geometry . fmap) (bimap f f)



deepFreeze :: Bootstrap () -> IO Technology
deepFreeze = views (frozen . from identity) (evaluate . force)


thread :: LSC IO (LSC IO a -> IO a)
thread = do
  f <- context
  pure $ either throwIO pure <=< f


semaphore :: LSC IO a -> LSC IO a
semaphore m = do
  f <- thread
  w <- view workers <$> environment
  case w of
    Nothing -> m
    Just sm -> indeterminate $ with sm $ f m


debug, info, warning :: Log Entry m => Entry -> LSC m ()
debug   = enter Debug
info    = enter Info
warning = enter Warning

