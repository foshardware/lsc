{-# LANGUAGE Arrows #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}

module LSC where

import Control.Arrow
import Control.Arrow.Algebraic
import Control.Arrow.Memo
import Control.Arrow.Select
import Control.Category
import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async
import qualified Control.Concurrent.MSem as MSem
import Control.Exception
import Control.Lens
import Control.Monad.Trans
import Control.Monad.Codensity hiding (improve)
import Data.Foldable
import Prelude hiding ((.), id)

import LSC.FastDP
import LSC.Legalize
import LSC.Mincut
import LSC.NetGraph
import LSC.Types
import LSC.Version



stage0 :: Compiler' NetGraph
stage0 = id
    >>> local gateGeometry
    >>> local assignCellsToRows
    >>> legalization >>> estimate
    >>> local assignCellsToColumns >>> estimate


stage1 :: Compiler' NetGraph
stage1 = zeroArrow


stage2 :: Compiler' NetGraph
stage2 = id
    >>> local gateGeometry >>> estimate
    >>> local assignCellsToRows >>> estimate
    >>> legalization >>> estimate
    >>> detailedPlacement >>> estimate
    >>> legalization >>> estimate
    >>> local assignCellsToColumns >>> estimate


stage4 :: Compiler' NetGraph
stage4 = zeroArrow



globalPlacement :: Compiler' NetGraph
globalPlacement = local placeQuad



detailedPlacement :: Compiler' NetGraph
detailedPlacement = id
    >>> local singleSegmentClustering >>> estimate
    >>> strategy1 significantHpwl
        (local globalSwap >>> local verticalSwap >>> legalization >>> local localReordering >>> legalization >>> estimate)
    >>> strategy1 significantHpwl
        (local singleSegmentClustering >>> estimate)



legalization :: Compiler' NetGraph
legalization = id
    >>> local juggleCells
    >>> local legalizeRows


estimate :: Compiler' NetGraph
estimate = proc top -> do
    () <- local estimations -< top
    returnA -< top




netGraph :: DAG Identifier NetGraph
netGraph = DAG (view identifier) subcells

circuit :: DAG Identifier RTL
circuit = DAG (view identifier) subcircuits



type Compiler' a = Compiler a a

type Compiler = LSR LS


compiler :: Compiler a b -> a -> LSC b
compiler = unLS . reduce



expensive :: (a -> b) -> Compiler a b
expensive f = local $ pure . f


remote_ :: LSC b -> Compiler () b
remote_ = remote . const

remote :: (a -> LSC b) -> Compiler a b
remote = LSR . Lift . LS


local_ :: LSC b -> Compiler () b
local_ = local . const

local :: (a -> LSC b) -> Compiler a b
local k = remote $ \ x -> do
  s <- thaw <$> technology
  o <- environment
  case o ^. workers of
    Singleton -> k x
    Workers i -> liftIO $ MSem.with i $ runLSC o s (k x)



strategy1 :: (a -> a -> Ordering) -> Compiler' a -> Compiler' a
strategy1 f a = proc p0 -> do

    ev <- remote_ environment -< ()

    let it = ev ^. iterations
    ps <- collect it f a -<< [p0]

    let p = minimumBy f ps

    case f p0 p of
        GT -> strategy1 f a -< p
        _  -> returnA -< p0


collect :: ArrowChoice a => Int -> (b -> b -> Ordering) -> a b b -> a [b] [b]
collect 0 _ _ = id
collect n f a = proc (p : ps) -> do
    q <- a -< p
    case f p q of
        EQ -> returnA -< p : ps
        _  -> collect (pred n) f a -< q : p : ps



env_ :: Setter' CompilerOpts o -> o -> Compiler a b -> Compiler a b
env_ setter = env setter . const

env :: Setter' CompilerOpts o -> (o -> o) -> Compiler a b -> Compiler a b
env setter f k = remote $ \ x -> do
  o <- environment
  let p = o & setter %~ f
  lift $ LST $ lift $ flip runEnvT p $ unLST $ lowerCodensity $ compiler k x


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
      Singleton -> liftIO $ (,) <$> runLSC o s (k x) <*> runLSC o s (m y)
      Workers _ -> liftIO $ concurrently (runLSC o s (k x)) (runLSC o s (m y))


instance ArrowSelect LS where
  select (LS k) = LS $ \ xs -> do
    s <- thaw <$> technology
    o <- environment
    case o ^. workers of
      Singleton -> mapM k xs
      Workers _ -> liftIO $ forConcurrently xs $ runLSC o s . k

instance ArrowRace LS where
  LS k /// LS m = LS $ \ (x, y) -> do
    s <- thaw <$> technology
    o <- environment
    case o ^. workers of
      Singleton -> Left <$> k x
      Workers _ -> liftIO $ do
        withAsync (runLSC o s (k x)) $ \ wx ->
          withAsync (runLSC o s (m y)) $ \ wy ->
            waitEitherCatch wx wy >>= \ xy -> case xy of

              Left (Right f) -> Left f <$ cancelWith wy (Fail "race lost")
              Left (Left (SomeException e)) -> do
                runLSC o s $ debug [displayException e]
                Right <$> wait wy

              Right (Right f) -> Right f <$ cancelWith wx (Fail "race lost")
              Right (Left (SomeException e)) -> do
                runLSC o s $ debug [displayException e]
                Left <$> wait wx


createWorkers :: Int -> IO Workers
createWorkers n | n < 2 = pure Singleton
createWorkers n = Workers <$> MSem.new n

rtsWorkers :: IO Workers
rtsWorkers = createWorkers =<< getNumCapabilities


instance ArrowZero LS where
  zeroArrow = LS $ const $ fail $ unwords ["start", versionString]

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
