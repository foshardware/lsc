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
import Control.Lens hiding (Traversing)
import Control.Monad.Trans
import Control.Monad.Codensity
import Prelude hiding ((.), id)

import LSC.Synthesis
import LSC.Placement
import LSC.Routing
import LSC.Types
import LSC.Version
import LSC.Web


stage1 :: Compiler' NetGraph
stage1 = zeroArrow
  <+> rpc routeWeb
  <+> dag netGraph (place >>> route)
  <+> dag netGraph (route & jogs `env` succ & rowSize `env` (+ 5000))


route :: Compiler' NetGraph
route = ls routeSat

place :: Compiler' NetGraph
place = ls placeEasy


synthesize :: Compiler' RTL
synthesize = ls synthesizeLogic

physical :: Compiler RTL NetGraph
physical = ls synthesizeGeometry


netGraph :: DAG Identifier NetGraph
netGraph = DAG (view identifier) subcells

circuit :: DAG Identifier RTL
circuit = DAG (view identifier) subcircuits


type Compiler' a = Compiler a a

type Compiler = LSR LS

compiler :: Compiler a b -> a -> LSC b
compiler = unLS . reduce


rpc :: (a -> LSC b) -> Compiler a b
rpc = remote . ls

ls_ :: LSC b -> Compiler b b
ls_ f = ls (<$ f)

ls :: (a -> LSC b) -> Compiler a b
ls = LSR . Lift . LS


env_ :: Simple Setter CompilerOpts o -> o -> Compiler a b -> Compiler a b
env_ setter = env setter . const

env :: Simple Setter CompilerOpts o -> (o -> o) -> Compiler a b -> Compiler a b
env setter f k = ls $ \ x -> do
  o <- environment
  let p = o & setter %~ f
  lift $ LST $ lift $ flip runEnvT p $ unLST $ lowerCodensity $ compiler k x


remote :: Compiler a b -> Compiler a b
remote act = ls $ \ x -> do
  s <- thaw <$> technology
  o <- environment
  case o ^. workers of
    Singleton -> compiler act x
    Workers i -> do
      liftIO $ MSem.signal i *> runLSC o s (compiler act x)
        `finally` MSem.wait i


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
      Workers i -> do
        liftIO $ concurrently
          (MSem.with i $ runLSC o s (k x))
          (runLSC o s (m y) `finally` MSem.signal i)


instance ArrowSelect LS where
  select (LS k) = LS $ \ xs -> do
    s <- thaw <$> technology
    o <- environment
    case o ^. workers of
      Singleton -> mapM k xs
      Workers i -> liftIO $ do
        MSem.signal i
        mapConcurrently (MSem.with i . runLSC o s . k) xs
          `finally` MSem.wait i


createWorkers :: Int -> IO Workers
createWorkers n | n < 2 = pure Singleton
createWorkers n = Workers <$> MSem.new (pred n)

rtsWorkers :: IO Workers
rtsWorkers = createWorkers =<< getNumCapabilities


instance ArrowZero LS where
  zeroArrow = throw $ AssertionFailed $ unwords ["start", versionString]

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

instance ArrowSelect ls => ArrowSelect (LSR ls) where
  select (LSR f) = LSR (select f)


instance ArrowPlus ls => ArrowZero (LSR ls) where
  zeroArrow = LSR zeroArrow

instance ArrowPlus ls => ArrowPlus (LSR ls) where
  LSR f <+> LSR g = LSR (mapReduce f <+> mapReduce g)

instance ArrowChoice ls => ArrowChoice (LSR ls) where
  LSR f +++ LSR g = LSR (mapReduce f +++ mapReduce g)
