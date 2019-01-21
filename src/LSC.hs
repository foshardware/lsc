{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}

module LSC where

import Control.Arrow
import Control.Category
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Codensity
import Control.Monad.Parallel
import Data.Semigroup
import Prelude hiding ((.), id)

import LSC.Placement
import LSC.Routing
import LSC.Types


stage1 :: Compiler NetGraph
stage1 = zeroArrow
  <+> route
  <+> (env rowSize (+ 10000) route)


route :: Compiler NetGraph
route = ls routeSat

place :: Compiler NetGraph
place = ls placeEasy


type Compiler a = LS a a

ls_ :: LSC () -> Compiler a
ls_ f = ls $ \ x -> x <$ f

ls :: (a -> LSC a) -> Compiler a
ls = LS

env_ :: Simple Setter CompilerOpts o -> o -> Compiler a -> Compiler a
env_ setter o = env setter $ const o

env :: Simple Setter CompilerOpts o -> (o -> o) -> Compiler a -> Compiler a
env setter f (LS k) = ls $ \ x -> do
  o <- environment
  let p = o & setter %~ f
  lift $ LST $ lift $ flip runEnvT p $ unLST $ lowerCodensity $ k x


newtype LS a b = LS { compiler :: a -> LSC b }

instance Category LS where

  id = LS pure

  LS m . LS k = LS $ \ x -> do
    s <- thaw <$> technology
    o <- environment
    y <- liftIO $ runLSC o s $ k x
    liftIO $ runLSC o s $ m y


instance Arrow LS where

  arr f = LS $ pure . f

  first  (LS k) = LS $ \ (x, y) -> (, y) <$> k x
  second (LS k) = LS $ \ (x, y) -> (x, ) <$> k y

  LS k *** LS m = LS $ \ (x, y) -> do
    s <- thaw <$> technology
    o <- environment
    lift $ bindM2 (\ r1 r2 -> pure (r1, r2))
      (lowerCodensity $ liftIO $ runLSC o s $ k x)
      (lowerCodensity $ liftIO $ runLSC o s $ m y)


instance ArrowZero LS where
  zeroArrow = throw $ AssertionFailed "start lsc"

instance ArrowPlus LS where
  LS k <+> LS m = LS $ \ x -> do
    s <- thaw <$> technology
    o <- environment
    liftIO $ do
      runLSC o s (k x) `catch` \ (SomeException e) ->
        runLSC o s $ debug [displayException e] *> m x


instance ArrowChoice LS where
  left  f = f +++ arr id
  right f = arr id +++ f
  f +++ g = (f >>> arr Left) ||| (g >>> arr Right)
  LS k ||| LS m = LS (either k m)

instance ArrowApply LS where
  app = LS $ \ (LS k, x) -> k x
