-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}

module LSC where

import Control.Arrow
import Control.Arrow.Algebraic
import Control.Arrow.Memo
import Control.Arrow.Select
import Control.Arrow.Transformer
import Control.Category
import Control.Concurrent.Async
import qualified Control.Concurrent.MSem as MSem
import Control.DeepSeq
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import Data.Functor.Contravariant
import Data.Semigroup
import Prelude hiding ((.), id)

import LSC.CellFlipping
import LSC.Estimate
import LSC.FastDP
import LSC.GlobalRouting
import LSC.Legalize
import LSC.Logger
import LSC.Model
import LSC.Mincut
import LSC.NetGraph
import LSC.Technology
import LSC.Transformer
import LSC.Version




stage3 :: Compiler' NetGraph
stage4 :: Compiler' NetGraph


stage3 = id

    >>> remote gateGeometry
    >>> arr rebuildHyperedges >>> estimate

    >>> detailedPlacement
    >>> strategy1 significantHpwl detailedPlacement

    >>> finalEstimate



stage4 = id

    >>> local determineRowSpacing
    >>> arr rebuildHyperedges >>> estimate

    >>> remote pinGeometry

    >>> local cellFlipping
    >>> estimate

    >>> remote pinGeometry

    >>> local determineNetSegments
    >>> finalEstimate

    -- >>> local determineRowSpacing
    -- >>> arr rebuildHyperedges >>> finalEstimate



globalPlacement :: Compiler' NetGraph
globalPlacement = local placeQuad



detailedPlacement :: Compiler' NetGraph
detailedPlacement = id

    >>> remote gateGeometry

    >>> arr assignCellsToRows
    >>> local juggleCells
    >>> local legalizeRows
    >>> arr rebuildHyperedges >>> estimate

    >>> fastDP >>> estimate

    >>> arr removeFeedthroughs
    >>> arr assignCellsToColumns
    >>> arr rebuildHyperedges >>> estimate

    >>> local legalizeRows
    >>> arr rebuildHyperedges >>> estimate

    >>> remote pinGeometry

    >>> local determineFeedthroughs
    >>> remote gateGeometry >>> estimate

    >>> local legalizeRows
    >>> arr rebuildHyperedges >>> estimate

    >>> strategy2 significantHpwl (local singleSegmentClustering >>> arr rebuildHyperedges)
    >>> estimate

    >>> local legalizeRows

    >>> arr assignCellsToColumns
    >>> arr rebuildHyperedges >>> estimate

    >>> arr removeFeedthroughs
    >>> arr assignCellsToColumns
    >>> arr rebuildHyperedges >>> estimate

    >>> remote pinGeometry

    >>> local determineFeedthroughs
    >>> remote gateGeometry >>> estimate

    >>> local legalizeRows
    >>> arr assignCellsToColumns
    >>> arr rebuildHyperedges >>> estimate




fastDP :: Compiler' NetGraph
fastDP = id
    -- >>> local singleSegmentClustering >>> arr rebuildHyperedges >>> estimate
    >>> strategy2 significantHpwl
        (id
        >>> estimate
        >>> local globalSwap
        >>> local legalizeRows >>> arr rebuildHyperedges
        >>> local verticalSwap
        >>> local legalizeRows >>> arr rebuildHyperedges
        >>> local localReordering
        >>> local legalizeRows >>> arr rebuildHyperedges
        >>> id)
    -- >>> strategy1 significantHpwl
    --     (local singleSegmentClustering >>> arr rebuildHyperedges >>> estimate)




estimate :: Compiler' NetGraph
estimate = proc top -> do
    remote estimations -< top
    returnA -< top


finalEstimate :: Compiler' NetGraph
finalEstimate = proc top -> do
    remote_ $ info ["Final estimate"] -< ()
    estimate -< top



netGraph :: DAG Identifier NetGraph
netGraph = DAG (view identifier) subcells

circuit :: DAG Identifier RTL
circuit = DAG (view identifier) subcircuits



newtype Endomorph c a = Endomorph { appEndomorph :: c a a }

instance Category c => Semigroup (Endomorph c a) where
  Endomorph g <> Endomorph f = Endomorph (g . f)

instance Category c => Monoid (Endomorph c a) where
  mempty = Endomorph id


type Compiler' a = Compiler a a

type Compiler = LSR LS

compiler :: Compiler a b -> a -> LSC b
compiler = unLS . (<+> noResult) . reduce


env :: Confinement () -> Compiler a b -> Compiler a b
env f k = remote $ confine f . unLS (reduce k)


remote_ :: LSC b -> Compiler () b
remote_ = remote . const

remote :: (a -> LSC b) -> Compiler a b
remote = lift . LS


local_ :: LSC b -> Compiler () b
local_ = local . const

local :: (a -> LSC b) -> Compiler a b
local k = remote $ \ x -> do
  s <- technology
  o <- environment
  case o ^. workers of
    Nothing -> k x
    Just sm -> liftIO $ MSem.with sm $ runLSC o s (k x)


expensive :: NFData b => (a -> b) -> Compiler a b
expensive f = local $ liftIO . evaluate . force f



type Strategy' a = Strategy a a

type Strategy a b = Compiler a b -> Compiler a b


strategy0 :: Int -> Strategy' a
strategy0 n = appEndomorph . stimes n . Endomorph


strategy1 :: Comparison a -> Strategy' a
strategy1 f a = proc x -> do
    i <- view iterations ^<< remote_ environment -< ()
    minimumBy (getComparison f) ^<< iterator1 i a -<< [x]


strategy2 :: Comparison a -> Strategy' a
strategy2 f a = proc x -> do
    y <- strategy1 f a -< x
    if getComparison f x y /= GT
    then returnA -< x
    else strategy2 f a -< y



iterator :: ArrowChoice a => Word -> a b b -> a [b] [b]
iterator i a = proc xs -> do
    if null xs
    then returnA -< xs
    else iterator1 i a -< xs


iterator1 :: Arrow a => Word -> a b b -> a [b] [b]
iterator1 0 _ = id
iterator1 i a = iterator1 (pred i) a <<< uncurry (:) ^<< first a <<^ head &&& id



-- | A Kleisli category with specialized arrow instances for concurrency and exception handling
--
newtype LS a b = LS { unLS :: a -> LSC b }

instance Category LS where

  id = LS pure
  LS k . LS m = LS (k <=< m)


instance Arrow LS where

  arr f = LS (pure . f)

  first  (LS k) = LS $ \ (x, y) -> (, y) <$> k x
  second (LS k) = LS $ \ (x, y) -> (x, ) <$> k y

  LS k *** LS m = LS $ \ (x, y) -> do
    s <- technology
    o <- environment
    case o ^. workers of
      Nothing -> (,) <$> k x <*> m y
      Just  _ -> liftIO $ concurrently (runLSC o s (k x)) (runLSC o s (m y))


instance ArrowSelect LS where
  select (LS k) = LS $ \ xs -> do
    s <- technology
    o <- environment
    case o ^. workers of
      Nothing -> mapM k xs
      Just  _ -> liftIO $ forConcurrently xs $ runLSC o s . k

instance ArrowRace LS where
  LS k /// LS m = LS $ \ (x, y) -> do
    s <- technology
    o <- environment
    case o ^. workers of
      Nothing -> Left <$> k x
      Just  _ -> liftIO $ do
        withAsync (runLSC o s (k x)) $ \ wx ->
          withAsync (runLSC o s (m y)) $ \ wy ->
            waitEitherCatch wx wy >>= \ xy -> case xy of

              Left (Right f) -> Left f <$ cancelWith wy LSZero
              Left (Left er) -> do
                runLSC o s $ logger Error [show @SomeException er]
                Right <$> wait wy

              Right (Right f) -> Right f <$ cancelWith wx LSZero
              Right (Left er) -> do
                runLSC o s $ logger Error [show @SomeException er]
                Left <$> wait wx


data NoResult = NoResult
  deriving Exception

instance Show NoResult where
  show _ = "no result, " ++ versionString ++ ", commit " ++ commitString

noResult :: LS a b
noResult = LS $ const $ liftIO $ throwIO NoResult


data LSZero = LSZero
  deriving (Exception, Show)

instance ArrowZero LS where
  zeroArrow = LS $ const $ liftIO $ throwIO LSZero

instance ArrowPlus LS where
  LS k <+> LS m = LS $ \ x -> do
    s <- technology
    o <- environment
    liftIO $ runLSC o s (k x) `catches`
      [ Handler $ \ LSZero -> runLSC o s $ m x
      , Handler $ \ er -> runLSC o s $ logger Error [show @SomeException er] *> m x
      ]


instance ArrowChoice LS where
  left  f = f +++ arr id
  right f = arr id +++ f
  f +++ g = (f >>> arr Left) ||| (g >>> arr Right)
  LS k ||| LS m = LS (either k m)

instance ArrowApply LS where
  app = LS $ \ (LS k, x) -> k x



-- | An arrow transformer with effects on the evaluation of the underlying arrow
--
newtype LSR ls a b = LSR { unLSR :: Algebraic ls a b }

reduce :: Arrow ls => LSR ls a b -> ls a b
reduce = algebraic . mapReduce . unLSR


instance Arrow ls => ArrowTransformer LSR ls where
  lift = LSR . Lift


instance Arrow ls => Category (LSR ls) where
  id = LSR id
  LSR f . LSR g = LSR (f . g)

instance Arrow ls => Arrow (LSR ls) where

  arr f = LSR (arr f)

  first  (LSR f) = LSR (first f)
  second (LSR f) = LSR (second f)

  LSR f *** LSR g = LSR (f *** g)


instance ArrowApply ls => ArrowApply (LSR ls) where
  app = lift $ arr (\ (f, x) -> (reduce f, x)) >>> app


instance ArrowSelect ls => ArrowSelect (LSR ls) where
  select (LSR f) = LSR (select (mapReduce f))

instance ArrowRace ls => ArrowRace (LSR ls) where
  LSR f /// LSR g = LSR (mapReduce f /// mapReduce g)


instance ArrowZero ls => ArrowZero (LSR ls) where
  zeroArrow = LSR zeroArrow

instance ArrowPlus ls => ArrowPlus (LSR ls) where
  LSR f <+> LSR g = LSR (mapReduce f <+> mapReduce g)

instance ArrowChoice ls => ArrowChoice (LSR ls) where
  LSR f +++ LSR g = LSR (mapReduce f +++ mapReduce g)

