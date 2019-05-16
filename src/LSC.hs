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
import Control.Monad.Codensity
import Prelude hiding ((.), id)

import LSC.Synthesis
import LSC.Easy
import LSC.Exline
import LSC.Force
import LSC.Integer
import LSC.NetGraph
import LSC.Types
import LSC.Version



stage1 :: Compiler' NetGraph
stage1 = globalPlacement



stage4 :: Compiler' NetGraph
stage4 = zeroArrow
  <+> dag netGraph (env_ rowSize 21000 route <+> route)



globalPlacement :: Compiler' NetGraph
globalPlacement = proc top -> do
  legalization <<< local (columns 64) -<< top



legalization :: Compiler' NetGraph
legalization = id
  >>> dag netGraph (remote placeColumn)
  >>> remote placeRows
  >>> arr inlineGeometry
  >>> remote contactGeometry



animatePlacement :: Compiler' NetGraph
animatePlacement = zeroArrow
  <+> local placeEasy >>> local placeForce



route :: Compiler' NetGraph
route = local routeInteger

place :: Compiler' NetGraph
place = local placeEasy


synthesize :: Compiler' RTL
synthesize = local synthesizeLogic

physical :: Compiler RTL NetGraph
physical = local synthesizeGeometry


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

remote_ :: LSC b -> Compiler' b
remote_ f = remote (<$ f)

remote :: (a -> LSC b) -> Compiler a b
remote = LSR . Lift . LS

local_ :: LSC b -> Compiler' b
local_ f = local (<$ f)

local :: (a -> LSC b) -> Compiler a b
local k = remote $ \ x -> do
  s <- thaw <$> technology
  o <- environment
  case o ^. workers of
    Singleton -> k x
    Workers i -> liftIO $ MSem.with i $ runLSC o s (k x)


env_ :: Simple Setter CompilerOpts o -> o -> Compiler a b -> Compiler a b
env_ setter = env setter . const

env :: Simple Setter CompilerOpts o -> (o -> o) -> Compiler a b -> Compiler a b
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
  zeroArrow = LS $ fail $ unwords ["start", versionString]

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
