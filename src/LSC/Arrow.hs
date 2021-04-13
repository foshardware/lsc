-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}

module LSC.Arrow where

import Control.Arrow
import Control.Arrow.Algebraic
import Control.Arrow.Select
import Control.Arrow.Transformer
import Control.Category
import Control.Concurrent.Async
import Control.DeepSeq
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Parallel
import Control.Parallel.Strategies (withStrategy, rseq, parTraversable)
import Data.Foldable
import Data.Functor.Contravariant
import Data.Profunctor
import Data.Semigroup
import Data.Vector (Vector, replicate)
import Prelude hiding ((.), id, replicate)

import LSC.Log
import LSC.Model
import LSC.Transformer
import LSC.Version



newtype Endomorph c a = Endomorph { appEndomorph :: c a a }

instance Category c => Semigroup (Endomorph c a) where
  Endomorph g <> Endomorph f = Endomorph (g . f)

instance Category c => Monoid (Endomorph c a) where
  mempty = Endomorph id


type Transpiler a = Compiler a a

type Compiler = LSR LS

compiler :: Compiler a b -> a -> LSC IO b
compiler = unLS . (<+> noResult) . reduce


env :: Confinement () -> Compiler a b -> Compiler a b
env f k = remote $ confine f . unLS (reduce k)

get :: LSC IO a -> Compiler (a -> b) b
get f = remote (<$> f)


induce :: (a -> LSC Identity b) -> Compiler a b
induce f = remote $ elevate generalize . f

expensive :: NFData b => (a -> b) -> Compiler a b
expensive f = local $ indeterminate . evaluate . force . f


remote :: (a -> LSC IO b) -> Compiler a b
remote = lift . LS

local :: (a -> LSC IO b) -> Compiler a b
local k = remote $ semaphore . k


inform :: Compiler String ()
inform = remote info <<^ pure



type Strategy a b = Compiler a b -> Compiler a b


strategy0 :: Word -> Strategy a a
strategy0 n = appEndomorph . stimes n . Endomorph


strategy1 :: Comparison a -> Strategy a a
strategy1 f a = proc x -> do
  i <- get environment -< view iterations
  minimumBy (getComparison f) ^<< iterator1 i a -<< [x]


strategy2 :: Comparison a -> Strategy a a
strategy2 f a = improve
  where
    improve = proc x -> do
      y <- strategy1 f a -< x
      if getComparison f x y /= GT
      then returnA -< x
      else improve -< y


strategy3 :: Word -> Comparison b -> Strategy a b
strategy3 n f a = improve <<< a &&& id
  where
    improve = proc (y, x) -> do
      z <- selector n (getComparison f) a -< x
      if getComparison f y z /= GT
      then returnA -< y
      else improve -< (z, x)




selector :: ArrowSelect a => Word -> (c -> c -> Ordering) -> a b c -> a b c
selector 0 _ _ = error "selector 0"
selector 1 _ a = a
selector n f a = minimumBy f ^<< select a <<^ vector n


amalgamator :: (ArrowSelect a, Monoid c) => Word -> a b c -> a b c
amalgamator 0 _ = arr $ const mempty
amalgamator 1 a = a
amalgamator 2 a = uncurry (<>) ^<< a &&& a
amalgamator n a = fold ^<< select a <<^ vector n


vector :: Word -> a -> Vector a
vector = replicate . fromIntegral



iterator :: ArrowChoice a => Word -> a b b -> a [b] [b]
iterator n a = proc xs -> do
    if null xs
    then returnA -< xs
    else iterator1 n a -< xs


iterator1 :: Arrow a => Word -> a b b -> a [b] [b]
iterator1 0 _ = id
iterator1 n a = iterator1 (pred n) a <<< uncurry (:) ^<< first a <<^ head &&& id




-- | A Kleisli category with specialized arrow instances for concurrency and exception handling
--
newtype LS a b = LS { unLS :: a -> LSC IO b }
  deriving Functor
  deriving (Category, ArrowChoice, ArrowApply) via Kleisli (LSC IO)
  deriving (Profunctor, Strong, Choice) via WrappedArrow LS


instance Arrow LS where

  arr f = LS (pure . f)

  first  (LS k) = LS $ \ (x, y) -> (, y) <$> k x
  second (LS k) = LS $ \ (x, y) -> (x, ) <$> k y

  LS m *** LS n = LS $ \ (x, y) -> do
    f <- thread
    g <- thread
    indeterminate $ f (m x) `concurrently` g (n y)


-- | Equivalent to profunctor traverse
--
instance ArrowSelect LS where
  select (LS k) = LS $ \ xs -> do
    f <- thread
    indeterminate $ forConcurrently xs (f . k)


-- | Concurrent races are inherently indeterminate 
--
instance ArrowRace LS where
  LS k /// LS m = LS $ \ (x, y) -> do
    f <- thread
    g <- thread
    indeterminate 
      $ withAsync (f (k x)) $ \ wx ->
        withAsync (g (m y)) $ \ wy ->

          waitEitherCatch wx wy >>= \ case

            Left (Right a) -> Left a <$ cancelWith wy LSZero
            Left (Left ex) -> do
              logSomeException ex
              Right <$> wait wy

            Right (Right a) -> Right a <$ cancelWith wx LSZero
            Right (Left ex) -> do
              logSomeException ex
              Left <$> wait wx



data NoResult = NoResult
  deriving Exception

instance Show NoResult where
  show = const $ "no result, " ++ versionString ++ ", tree " ++ commitString

noResult :: LS a b
noResult = LS $ const $ throwLSC NoResult


data LSZero = LSZero
  deriving (Exception, Show)


instance ArrowZero LS where
  zeroArrow = LS $ const $ throwLSC LSZero


instance ArrowPlus LS where
  LS k <+> LS m = LS $ \ x -> do
    f <- thread
    indeterminate 
      $ f (k x) `catches`
        [ Handler $ \ LSZero -> f (m x)
        , Handler $ \ ex -> logSomeException ex >> f (m x)
        ]




-- | Parallelism without concurrency
--
newtype a +> b = LSP { unLSP :: a -> b }
  deriving Functor
  deriving (Category, ArrowChoice, ArrowApply, ArrowLoop) via (->)
  deriving (Profunctor, Strong, Choice, Costrong) via WrappedArrow (+>)


parallel :: Iso (a +> b) (c +> d) (a -> b) (c -> d)
parallel = iso unLSP LSP


instance Arrow (+>) where

  arr f = LSP f

  LSP f *** LSP g = LSP $ \ (x, y) ->
    let a = f x
        b = g y
    in (par b a, par a b)


instance ArrowSelect (+>) where
  select (LSP f) = LSP
    $ withStrategy (parTraversable rseq)
    . fmap f




-- | An arrow transformer with effects on the evaluation of the underlying arrow
--
newtype LSR ls a b = LSR { unLSR :: Algebraic ls a b }
  deriving (Category, Arrow) via Algebraic ls
  deriving (Profunctor, Strong, Choice) via WrappedArrow (LSR ls)


reduce :: Arrow ls => LSR ls a b -> ls a b
reduce = algebraic . mapReduce . unLSR


instance Arrow ls => ArrowTransformer LSR ls where
  lift = LSR . Lift


instance ArrowApply ls => ArrowApply (LSR ls) where
  app = lift $ first reduce ^>> app


instance ArrowZero ls => ArrowZero (LSR ls) where
  zeroArrow = LSR zeroArrow

instance ArrowPlus ls => ArrowPlus (LSR ls) where
  LSR f <+> LSR g = LSR (mapReduce f <+> mapReduce g)

instance ArrowChoice ls => ArrowChoice (LSR ls) where
  LSR f +++ LSR g = LSR (mapReduce f +++ mapReduce g)


instance ArrowSelect ls => ArrowSelect (LSR ls) where
  select (LSR f) = LSR (select (mapReduce f))

instance ArrowRace ls => ArrowRace (LSR ls) where
  LSR f /// LSR g = LSR (mapReduce f /// mapReduce g)

