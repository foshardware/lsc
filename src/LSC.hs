-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}

module LSC where

import Control.Arrow
import Control.Arrow.Algebraic
import Control.Arrow.Memo
import Control.Arrow.Select
import Control.Category
import Control.Concurrent.Async
import qualified Control.Concurrent.MSem as MSem
import Control.DeepSeq
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Data.Foldable
import Prelude hiding ((.), id)

import LSC.CellFlipping
import LSC.FastDP
import LSC.GlobalRouting
import LSC.Legalize
import LSC.Logger
import LSC.Mincut
import LSC.NetGraph
import LSC.Trace
import LSC.Types
import LSC.Version




stage1 :: Compiler' NetGraph
stage1 = zeroArrow



stage2 :: Compiler' NetGraph
stage2 = id
    >>> remote gateGeometry
    >>> arr rebuildHyperedges >>> estimate
    >>> stage2'
    >>> strategy1 significantHpwl stage2'
    >>> finalEstimate


stage2' :: Compiler' NetGraph
stage2' = id

    >>> remote gateGeometry
    >>> legalization >>> estimate

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




stage3 :: Compiler' NetGraph
stage3 = id

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



stage4 :: Compiler' NetGraph
stage4 = zeroArrow



globalPlacement :: Compiler' NetGraph
globalPlacement = local placeQuad



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



legalization :: Compiler' NetGraph
legalization = id
    >>> arr assignCellsToRows
    >>> local juggleCells
    >>> local legalizeRows
    >>> arr rebuildHyperedges



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



type Compiler' a = Compiler a a

type Compiler = LSR LS


compiler :: Compiler a b -> a -> LSC b
compiler = unLS . (<+> zeroArrow) . reduce


remote_ :: LSC b -> Compiler () b
remote_ = remote . const

remote :: (a -> LSC b) -> Compiler a b
remote = LSR . Lift . LS


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


instance Show a => Trace (Compiler a) a where
  trace = const $ remote trace


type Strategy a = Compiler' a -> Compiler' a


strategy0 :: Int -> Strategy a
strategy0 n = foldl (>>>) id . replicate n


strategy1 :: (a -> a -> Ordering) -> Strategy a
strategy1 f a = proc x -> do
    i <- view iterations ^<< remote_ environment -< ()
    minimumBy f ^<< iterator i a -<< [x]


strategy2 :: (a -> a -> Ordering) -> Strategy a
strategy2 f a = proc x -> do
    y <- strategy1 f a -< x
    if f x y /= GT
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



env_ :: Setter' CompilerOpts o -> o -> Compiler a b -> Compiler a b
env_ setter = env setter . const

env :: Setter' CompilerOpts o -> (o -> o) -> Compiler a b -> Compiler a b
env setter f k = remote $ modifyEnv (setter %~ f) . unLS (reduce k)



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


data LSZero = LSZero
  deriving Exception

instance Show LSZero where
  show = const $ "no result, " ++ versionString

instance ArrowZero LS where
  zeroArrow = LS $ \ _ -> liftIO $ throwIO LSZero

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


instance ArrowApply ls => ArrowApply (LSR ls) where
  app = LSR $ Lift $ arr (\ (f, x) -> (reduce f, x)) >>> app


instance ArrowSelect ls => ArrowSelect (LSR ls) where
  select (LSR f) = LSR (select (mapReduce f))

instance ArrowRace ls => ArrowRace (LSR ls) where
  LSR f /// LSR g = LSR (mapReduce f /// mapReduce g)


instance ArrowPlus ls => ArrowZero (LSR ls) where
  zeroArrow = LSR zeroArrow

instance ArrowPlus ls => ArrowPlus (LSR ls) where
  LSR f <+> LSR g = LSR (mapReduce f <+> mapReduce g)

instance ArrowChoice ls => ArrowChoice (LSR ls) where
  LSR f +++ LSR g = LSR (mapReduce f +++ mapReduce g)

