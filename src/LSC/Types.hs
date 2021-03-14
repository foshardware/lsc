-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}

module LSC.Types where

import Control.Applicative
import Control.Lens hiding (element)
import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.MSem (MSem)
import qualified Control.Concurrent.MSem as MSem
import Control.DeepSeq
import Control.Exception
import Control.Monad.ST
import Data.Bits
import Data.Copointed
import Data.Default
import Data.Foldable
import Data.Function
import Data.HashMap.Lazy (HashMap, unionWith)
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.List (sort, group, groupBy)
import Data.Semigroup
import Data.String
import Data.Text (Text)
import qualified Data.Text.Lazy as Lazy
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int
import Data.Vector (Vector)

import Data.Aeson (FromJSON, ToJSON)

import Control.Monad.IO.Class
import Control.Monad.Codensity
import Control.Monad.Morph
#if MIN_VERSION_base(4,10,0)
import Control.Monad hiding (fail)
import Control.Monad.Fail
import Control.Monad.Reader hiding (fail)
import Control.Monad.State hiding (fail)
#else
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
#endif

import GHC.Generics
import Prelude hiding (lookup, fail)
import System.IO

import LSC.Component
import LSC.Logger
import LSC.Trace
import LSC.Polygon



data RTL = RTL
  { _identifier  :: Identifier
  , _description :: AbstractGate
  , _subcircuits :: HashMap Identifier RTL
  } deriving (Generic, Show)

instance Default RTL where
  def = RTL
      { _identifier  = mempty
      , _description = mempty
      , _subcircuits = mempty
      }


data AbstractGate = AbstractGate [LogicPort] [Expr]
  deriving (Generic, Show)

instance Semigroup AbstractGate where
  AbstractGate ps es <> AbstractGate qs fs = AbstractGate (ps <> qs) (es <> fs)

instance Monoid AbstractGate where
  mempty = AbstractGate mempty mempty
#if !MIN_VERSION_base(4,11,0)
  mappend = (<>)
#endif


data LogicPort = LogicPort
  { _identifier :: Identifier
  , _dir        :: Dir
  } deriving (Generic, Show)


data Expr
  = Assign Identifier Expr
  | Ref Identifier
  | And [Expr]
  deriving (Generic, Show)



data NetGraph = NetGraph
  { _identifier  :: Identifier
  , _supercell   :: AbstractCell
  , _subcells    :: HashMap Identifier NetGraph
  , _gates       :: Vector Gate
  , _nets        :: HashMap Identifier Net
  } deriving (Generic, NFData, FromJSON, ToJSON, Show)

instance Default NetGraph where
  def = NetGraph
      { _identifier = mempty
      , _supercell  = def
      , _subcells   = mempty
      , _gates      = mempty
      , _nets       = mempty
      }


data Net = Net
  { _identifier  :: Identifier
  , _geometry    :: [Polygon' Layer Int]
  , _netSegments :: [Line' Int]
  , _members     :: Vector Gate
  , _contacts    :: HashMap Number [Pin]
  } deriving (Generic, NFData, FromJSON, ToJSON, Show)


instance Semigroup Net where
  Net "" xs ss ns as <> Net is ys ts os bs
    = Net is (xs <> ys) (ss <> ts) (ns <> os) (unionWith (<>) as bs)
  Net is xs ss ns as <> Net  _ ys ts os bs
    = Net is (xs <> ys) (ss <> ts) (ns <> os) (unionWith (<>) as bs)

instance Monoid Net where
  mempty = Net "" mempty mempty mempty mempty
#if !MIN_VERSION_base(4,11,0)
  mappend = (<>)
#endif



type Number = Int

type Identifier = Text

base16Identifier :: Int -> Identifier
base16Identifier
  = head
  . Lazy.toChunks
  . toLazyTextWith (finiteBitSize (0 :: Int) `shiftR` 2)
  . hexadecimal



data Gate = Gate
  { _identifier  :: Identifier
  , _space       :: Component' Layer Int
  , _wires       :: HashMap Identifier Identifier
  , _number      :: Number  -- ^ pin     ^ net
  , _fixed       :: Bool
  , _feedthrough :: Bool
  } deriving (Eq, Generic, NFData, FromJSON, ToJSON, Show)

instance Default Gate where
  def = Gate
      { _identifier  = mempty
      , _space       = mempty
      , _wires       = mempty
      , _number      = -1
      , _fixed       = False
      , _feedthrough = False
      }


data Track = Track
  { _stabs :: IntSet
  , _z     :: IntSet
  } deriving (Generic, NFData, FromJSON, ToJSON, Show)



data Row = Row
  { _identifier  :: Identifier
  , _number      :: Number 
  , _l           :: Int
  , _b           :: Int
  , _orientation :: Orientation
  , _cardinality :: Int
  , _granularity :: Int
  } deriving (Generic, NFData, FromJSON, ToJSON, Show)

instance Default Row where
    def = Row
        { _identifier  = mempty
        , _number      = -1
        , _l           = 0
        , _b           = 0
        , _orientation = mempty
        , _cardinality = 0
        , _granularity = 1
        }


data AbstractCell = AbstractCell
  { _geometry  :: [Component' Layer Int]
  , _tracks    :: [Either Track Track]
  , _rows      :: IntMap Row
  , _vdd       :: Pin
  , _gnd       :: Pin
  , _pins      :: HashMap Identifier Pin
  } deriving (Generic, NFData, FromJSON, ToJSON, Show)

instance Default AbstractCell where
  def = AbstractCell
      { _geometry = mempty
      , _tracks   = mempty
      , _rows     = mempty
      , _vdd      = def
      , _gnd      = def
      , _pins     = mempty
      }


data Cell = Cell
  { _pins       :: HashMap Identifier Pin
  , _vdd        :: Pin
  , _gnd        :: Pin
  , _dims       :: (Int, Int)
  } deriving (Generic, FromJSON, ToJSON, Show)

instance Default Cell where
  def = Cell
      { _pins = mempty
      , _vdd  = def
      , _gnd  = def
      , _dims = def
      }



type Port = Polygon' Layer Int


data Dir = In | Out | InOut
  deriving (Eq, Generic, NFData, FromJSON, ToJSON, Show)


data Pin = Pin
  { _identifier :: Identifier
  , _dir        :: Maybe Dir
  , _geometry   :: [Port]
  } deriving (Generic, NFData, FromJSON, ToJSON, Show)

instance Default Pin where
  def = Pin
      { _identifier = mempty
      , _dir        = Nothing
      , _geometry   = mempty
      }



data Technology = Technology
  { _scaleFactor    :: Double
  , _stdCells       :: HashMap Identifier Cell
  } deriving (Generic, FromJSON, ToJSON, Show)


instance Default Technology where
  def = Technology
      { _scaleFactor = 1
      , _stdCells    = mempty
      }


type BootstrapT = StateT Technology

type Bootstrap = BootstrapT Identity

bootstrap :: Monad m => (Technology -> Technology) -> BootstrapT m ()
bootstrap = modify

snapshot :: Monad m => BootstrapT m Technology
snapshot = get

frozen :: Monad m => Iso' (BootstrapT m ()) (m Technology)
frozen = iso (flip execStateT def) (put <=< lift)

freeze :: Bootstrap () -> Technology
freeze = copoint . view frozen


type GnosticT = ReaderT Technology

type Gnostic = GnosticT Identity

runGnosticT :: Technology -> GnosticT m r -> m r
runGnosticT = flip runReaderT

runGnostic :: Technology -> Gnostic r -> r
runGnostic x = copoint . runGnosticT x



type Workers = MSem Word

createWorkers :: Word -> IO (Maybe Workers)
createWorkers 0 = pure Nothing
createWorkers 1 = pure Nothing
createWorkers n = Just <$> MSem.new n

rtsWorkers :: IO (Maybe Workers)
rtsWorkers = createWorkers . fromIntegral . max 0 =<< getNumCapabilities


data CompilerOpts = CompilerOpts
  { _rowCapacity   :: Double
  , _logLevel      :: LogLevel
  , _enableVisuals :: Bool
  , _iterations    :: Word
  , _workers       :: Maybe Workers
  , _seedHandle    :: Maybe Handle
  }

instance Default CompilerOpts where
  def = CompilerOpts
      { _rowCapacity   = 1
      , _logLevel      = Warning
      , _enableVisuals = False
      , _iterations    = 1
      , _workers       = Nothing
      , _seedHandle    = Nothing
      }


type EnvironmentT = ReaderT CompilerOpts

type Environment = EnvironmentT Identity

runEnvironmentT :: Monad m => CompilerOpts -> EnvironmentT m r -> m r
runEnvironmentT = flip runReaderT

runEnvironment :: CompilerOpts -> Environment r -> r
runEnvironment x = copoint . runEnvironmentT x



type LSC = Codensity (LST IO)

runLSC :: CompilerOpts -> Technology -> LSC a -> IO a
runLSC opts tech
  = runGnosticT tech
  . runEnvironmentT opts
  . unLST
  . lowerCodensity


evalLSC :: CompilerOpts -> Technology -> LSC a -> IO a
evalLSC = runLSC


realWorldST :: ST RealWorld a -> LSC a
realWorldST = liftIO . stToIO


technology :: LSC Technology
technology = lift $ LST $ lift ask

environment :: LSC CompilerOpts
environment = lift $ LST ask

modifyEnv :: (CompilerOpts -> CompilerOpts) -> LSC a -> LSC a
modifyEnv f = lift . LST . local f . unLST . lowerCodensity



newtype LST m a = LST { unLST :: EnvironmentT (GnosticT m) a }
  deriving Functor 


instance Applicative m => Applicative (LST m) where
  pure = LST . pure
  LST x <*> LST y = LST (x <*> y)

instance Monad m => Monad (LST m) where
  return = LST . return
  m >>= k = LST (unLST m >>= unLST . k)


instance MFunctor LST where
  hoist m = LST . hoist (hoist m) . unLST

instance MonadTrans LST where
  lift = LST . lift . lift


instance MonadIO m => MonadIO (LST m) where
  liftIO = LST . liftIO

#if MIN_VERSION_base(4,10,0)
instance MonadIO m => MonadFail (LST m) where
  fail = catchFail . Fail

newtype Fail = Fail { unFail :: String }
  deriving Exception

instance Show Fail where
  show = unFail


catchFail :: MonadIO m => Fail -> LST m a
catchFail = liftIO . throwIO

assume :: MonadFail m => String -> Bool -> m ()
assume = flip unless . fail
#else
assume :: Monad m => String -> Bool -> m ()
assume = flip unless . fail
#endif


instance (MonadIO m, Show a) => Trace (LST m) a where
  trace = liftIO . trace
  {-# INLINE trace #-}

instance Show a => Trace LSC a where
  trace = lift . trace
  {-# INLINE trace #-}



makeFieldsNoPrefix ''RTL

makeFieldsNoPrefix ''LogicPort

makeFieldsNoPrefix ''NetGraph

makeFieldsNoPrefix ''Track

makeFieldsNoPrefix ''Row

makeFieldsNoPrefix ''AbstractCell

makeFieldsNoPrefix ''Cell

makeFieldsNoPrefix ''Pin

makeFieldsNoPrefix ''Gate

makeFieldsNoPrefix ''Net

makeFieldsNoPrefix ''CompilerOpts

makeFieldsNoPrefix ''Technology



newtype DistinctNumber n a = DistinctNumber { getDistinctNumber :: a }
  deriving (Functor, Show)

instance (Eq n, HasNumber a n) => Eq (DistinctNumber n a) where
    DistinctNumber x == DistinctNumber y
      = view number x == view number y

instance (Ord n, HasNumber a n) => Ord (DistinctNumber n a) where
    DistinctNumber x `compare` DistinctNumber y
      = view number x `compare` view number y


newtype DistinctIdentifier i a = DistinctIdentifier { getDistinctIdentifier :: a }
  deriving (Functor, Show)

instance (Eq i, HasIdentifier a i) => Eq (DistinctIdentifier i a) where
    DistinctIdentifier x == DistinctIdentifier y
      = view identifier x == view identifier y

instance (Ord i, HasIdentifier a i) => Ord (DistinctIdentifier i a) where
    DistinctIdentifier x `compare` DistinctIdentifier y
      = view identifier x `compare` view identifier y




layers :: (HasZ a IntSet, Enum l) => Lens' a [l]
layers = lens
    (map toEnum . IntSet.toList . view z)
    (flip $ set z . IntSet.fromList . map fromEnum)
{-# INLINABLE layers #-}



rescale :: Double -> Bootstrap ()
rescale m = do
    k <- view scaleFactor <$> snapshot
    scaleFactor .= m
    let f = round . (/ k) . (* m) . fromIntegral
    stdCells %= (fmap . over vdd . over geometry . fmap) (bimap f f)
    stdCells %= (fmap . over gnd . over geometry . fmap) (bimap f f)
    stdCells %= (fmap . over dims) (bimap f f)
    stdCells %= (fmap . over pins . fmap . over geometry . fmap) (bimap f f)




debug, info, warning :: [String] -> LSC ()
debug   = logger Debug
info    = logger Info
warning = logger Warning


logger :: LogLevel -> [String] -> LSC ()
logger k xs = do
    level <- view logLevel <$> environment
    when (k <= level) $ liftIO $ logStderr k xs




-- | Operations on the list type
--

distinctPairs :: [a] -> [(a, a)]
distinctPairs (x : xs) = zip (repeat x) xs ++ distinctPairs xs
distinctPairs _ = []



unstableUnique :: Ord a => [a] -> [a]
unstableUnique = map head . group . sort
{-# INLINABLE unstableUnique #-}


median :: Integral a => [a] -> a
median
    = uncurry div
    . foldl (\ (a, len) x -> (a + x, len + 1)) (0, 0)
    . medianElements
{-# INLINABLE median #-}


medianElements :: [a] -> [a]
medianElements zs = go zs zs
    where go (x : _)         (_ : []) = [x]
          go (x : y : _) (_ : _ : []) = [x, y]
          go (_ : xs)    (_ : _ : ys) = go xs ys
          go _ _ = error "medianElements: empty list"



groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn = groupBy . on (==)
{-# INLINABLE groupOn #-}


-- | Assorted higher-order functions
--

ifoldl' :: Foldable f => (Int -> b -> a -> b) -> b -> f a -> b
ifoldl' f y xs = foldl' (\ g x !i -> f i (g (i - 1)) x) (const y) xs (length xs - 1)
{-# INLINE ifoldl' #-}

#if !MIN_VERSION_base(4,13,0)
foldMap' :: (Foldable f, Monoid m) => (a -> m) -> f a -> m
foldMap' f = foldl' (\ acc a -> acc `mappend` f a) mempty
{-# INLINE foldMap' #-}
#endif

