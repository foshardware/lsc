{-# LANGUAGE Arrows #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}

module LSC where

import Control.Arrow
import Control.Arrow.Algebraic
import Control.Arrow.Select
import Control.Category
import Control.Concurrent.Async
import Control.Exception
import Control.Lens
import Control.Monad.Trans
import Control.Monad.Codensity
import Prelude hiding ((.), id)

import LSC.Placement
import LSC.Routing
import LSC.Types


stage1 :: Compiler NetGraph
stage1 = zeroArrow
  <+> hierarchical (place >>> route)
  <+> env rowSize (+ 10000) route


hierarchical :: Compiler NetGraph -> Compiler NetGraph
hierarchical act = proc net -> do
  models <- select $ hierarchical act -< net ^. subcells
  act -< net & subcells .~ models


route :: Compiler NetGraph
route = ls routeSat

place :: Compiler NetGraph
place = ls placeEasy


type Compiler a = LSR LS a a

compiler :: Compiler a -> a -> LSC a
compiler = unLS . reduce


ls_ :: LSC b -> Compiler a
ls_ f = ls (<$ f)

ls :: (a -> LSC a) -> Compiler a
ls = LSR . Lift . LS

env_ :: Simple Setter CompilerOpts o -> o -> Compiler a -> Compiler a
env_ setter o = env setter $ const o

env :: Simple Setter CompilerOpts o -> (o -> o) -> Compiler a -> Compiler a
env setter f k = ls $ \ x -> do
  o <- environment
  let p = o & setter %~ f
  lift $ LST $ lift $ flip runEnvT p $ unLST $ lowerCodensity $ compiler k x


remote :: Compiler a -> Compiler a
remote act = ls $ \ x -> do
  s <- thaw <$> technology
  o <- environment
  pushWorker
  liftIO $ runLSC o s (compiler act x)
    `finally` runLSC o s (popWorker)


newtype LS a b = LS { unLS :: a -> LSC b }

instance Category LS where

  id = LS pure

  LS m . LS k = LS $ \ x -> do
    s <- thaw <$> technology
    o <- environment
    y <- liftIO $ runLSC o s $ k x
    liftIO $ runLSC o s $ m y


instance Arrow LS where

  arr f = LS (pure . f)

  first  (LS k) = LS $ \ (x, y) -> (, y) <$> k x
  second (LS k) = LS $ \ (x, y) -> (x, ) <$> k y

  LS k *** LS m = LS $ \ (x, y) -> do
    s <- thaw <$> technology
    o <- environment
    case o ^. workers of
      Singleton -> do
        liftIO $ (,) <$> runLSC o s (k x) <*> runLSC o s (m y)
      Workers _ -> do
        liftIO $ concurrently
          (runLSC o s (popWorker *> k x) `finally` runLSC o s pushWorker)
          (runLSC o s (m y) `finally` runLSC o s pushWorker)


instance ArrowZero LS where
  zeroArrow = throw $ AssertionFailed "start lsc"

instance ArrowPlus LS where
  LS k <+> LS m = LS $ \ x -> do
    s <- thaw <$> technology
    o <- environment
    liftIO $ do
      runLSC o s (k x) `catch` \ (SomeException e) ->
        runLSC o s (debug [displayException e] *> m x)


instance ArrowChoice LS where
  left  f = f +++ arr id
  right f = arr id +++ f
  f +++ g = (f >>> arr Left) ||| (g >>> arr Right)
  LS k ||| LS m = LS (either k m)

instance ArrowApply LS where
  app = LS $ \ (LS k, x) -> k x


newtype LSR ls a b = LSR { unLSR :: Algebraic ls a b }

reduce :: Arrow ls => LSR ls a b -> ls a b
reduce = algebraic . mapReduce . unLSR

instance Arrow ls => Category (LSR ls) where
  id = LSR id
  LSR f . LSR g = LSR (f . g)

instance Arrow ls => Arrow (LSR ls) where

  arr f = LSR (arr f)

  first  (LSR f) = LSR (first f)
  second (LSR f) = LSR (second f)

  LSR f *** LSR g = LSR (f *** g)


instance ArrowPlus ls => ArrowZero (LSR ls) where
  zeroArrow = LSR zeroArrow

instance ArrowPlus ls => ArrowPlus (LSR ls) where
  LSR f <+> LSR g = LSR (mapReduce f <+> mapReduce g)

instance ArrowChoice ls => ArrowChoice (LSR ls) where
  LSR f +++ LSR g = LSR (mapReduce f +++ mapReduce g)
