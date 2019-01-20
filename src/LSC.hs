{-# LANGUAGE TupleSections #-}

module LSC where

import Control.Arrow
import Control.Category
import Control.Exception
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
  <+> (increaseRowSize  5000 >>> route)
  <+> (increaseRowSize 10000 >>> route)


route :: Compiler NetGraph
route = ls routeSat

place :: Compiler NetGraph
place = ls placeEasy


increaseRowSize :: Int -> Compiler a
increaseRowSize n = ls_ $ modifyEnv rowSize (+ fromIntegral n)


type Compiler a = LS a a

ls_ :: LSC () -> Compiler a
ls_ f = ls $ \ x -> x <$ f

ls :: (a -> LSC a) -> Compiler a
ls f = LS $ lowerCodensity . f

compiler :: Compiler a -> a -> LSC a
compiler (LS k) = lift . k

newtype LS a b = LS (a -> LST b)

instance Category LS where
  id = LS pure
  LS m . LS k = LS $ \ x -> lowerCodensity $ do
    s <- thaw <$> technology
    o <- environment
    (x', s') <- liftIO $ runLSC o s $ lift $ m =<< k x
    x' <$ overwrite s'

instance Arrow LS where
  arr f = LS $ pure . f

  first (LS k) = LS $ \ (x, y) -> (, y) <$> k x

  LS k &&& LS m = LS $ \ x -> lowerCodensity $ do
    s <- thaw <$> technology
    o <- environment
    lift $ bindM2
      (\ (r1, s1) (r2, s2) -> (r1, r2) <$ lowerCodensity (overwrite $ s1 <> s2))
      (lowerCodensity $ liftIO $ runLSC o s $ lift $ k x)
      (lowerCodensity $ liftIO $ runLSC o s $ lift $ m x)

  LS k *** LS m = LS $ \ (x, y) -> lowerCodensity $ do
    s <- thaw <$> technology
    o <- environment
    lift $ bindM2
      (\ (r1, s1) (r2, s2) -> (r1, r2) <$ lowerCodensity (overwrite $ s1 <> s2))
      (lowerCodensity $ liftIO $ runLSC o s $ lift $ k x)
      (lowerCodensity $ liftIO $ runLSC o s $ lift $ m y)


instance ArrowZero LS where
  zeroArrow = throw $ AssertionFailed "start lsc"

instance ArrowPlus LS where
  LS k <+> LS m = LS $ \ x -> lowerCodensity $ do
    s <- thaw <$> technology
    o <- environment
    (x', s') <- liftIO $ do
      (runLSC o s $ lift $ k x) `catch` \ (SomeException e) -> do
        _ <- runLSC o s $ debug [displayException e]
        runLSC o s $ lift $ m x
    x' <$ overwrite s'


instance ArrowChoice LS where
  left  f = f +++ arr id
  right f = arr id +++ f
  f +++ g = (f >>> arr Left) ||| (g >>> arr Right)
  LS k ||| LS m = LS (either k m)

instance ArrowApply LS where
  app = LS $ \ (LS k, x) -> k x
