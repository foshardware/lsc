{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DuplicateRecordFields #-}


module LSC.Types where

import Control.Lens hiding (element)
import Data.Default
import Data.Foldable
import Data.Function (on)
import Data.Map (Map, unionWith, lookup)
import Data.Semigroup
import Data.Text (Text)
import Data.Vector (Vector)

import Control.Monad.Codensity
import Control.Monad.Parallel (MonadFork(..), MonadParallel(..))
import qualified Control.Monad.Parallel as Par
import Control.Monad.Reader (ReaderT(..), Reader, runReader)
import qualified Control.Monad.Reader as Reader
import Control.Monad.State
import Control.Parallel (par)

import Data.Time.Clock.POSIX

import Prelude hiding (lookup)

import Data.SBV

import System.Console.Concurrent


data NetGraph = NetGraph
  { _identifier  :: Identifier
  , _supercell   :: AbstractGate
  , _subcells    :: Map Identifier NetGraph
  , _gates       :: Vector Gate
  , _nets        :: Map Identifier Net
  } deriving Show

type Contact = Pin

data Net = Net
  { _identifier :: Identifier
  , _geometry   :: Path
  , _contacts   :: Map Gate [Contact]
  } deriving Show

type Identifier = Text

data Gate = Gate
  { _identifier :: Identifier
  , _geometry   :: Path
  , _vdd        :: Pin
  , _gnd        :: Pin
  , _wires      :: Map Identifier Identifier
  , _integer    :: Int
  } deriving Show


data AbstractGate = AbstractGate
  { _geometry  :: Path
  , _vdd       :: Pin
  , _gnd       :: Pin
  , _pins      :: Map Identifier Pin
  } deriving Show

data Cell = Cell
  { _pins       :: Map Identifier Pin
  , _vdd        :: Pin
  , _gnd        :: Pin
  , _dims       :: (Integer, Integer)
  } deriving Show

data Pin = Pin
  { _identifier :: Identifier
  , _dir        :: Dir
  , _ports      :: [Port]
  } deriving Show

type Port = Component Layer Integer


data Dir = In | Out | InOut
  deriving (Eq, Show)

data Layer
  = AnyLayer
  | Metal1
  | Metal2
  | Metal3
  deriving (Eq, Ord, Enum, Read, Show)

metal1, metal2, metal3 :: SLayer
metal1   = slayer Metal1
metal2   = slayer Metal2
metal3   = slayer Metal3

slayer :: Layer -> SLayer
slayer = literal . toEnum . fromEnum


data Technology = Technology
  { _scaleFactor    :: Double
  , _featureSize    :: Double
  , _stdCells       :: Map Text Cell
  , _standardPin    :: (Integer, Integer)
  , _rowSize        :: Integer
  , _enableDebug    :: Bool
  } deriving Show


type BootstrapT m = StateT Technology m
type Bootstrap = State Technology

bootstrap :: (Technology -> Technology) -> Bootstrap ()
bootstrap = modify

freeze :: Bootstrap () -> Technology
freeze bootstrapping = execState bootstrapping def

thaw :: Technology -> Bootstrap ()
thaw = put


type GnosticT m = ReaderT Technology m
type Gnostic = Reader Technology

runGnosticT :: GnosticT m r -> Technology -> m r
runGnosticT = runReaderT

gnostic :: Bootstrap () -> Gnostic r -> r
gnostic b a = a `runReader` freeze b


type LSC = Codensity LST

newtype LST a = LST { unLST :: GnosticT Symbolic a }

instance Functor LST where
  fmap f (LST a) = LST (fmap f a)

instance Applicative LST where
  pure = LST . pure
  LST a <*> LST b = LST (a <*> b)

instance Monad LST where
  return = pure
  m >>= k = LST (unLST m >>= unLST . k)

instance MonadIO LST where
  liftIO = LST . liftIO

instance MonadFork LST where
  forkExec (LST m) = do
    fmap liftIO . liftIO
      . withConcurrentOutput
      . forkExec
      . runSMT . runGnosticT m
      =<< LST Reader.ask

instance MonadParallel LST where
  bindM2 f ma mb = do
    wb <- forkExec mb
    a <- ma
    b <- wb
    f a b

runLSC :: Bootstrap () -> LSC a -> IO a
runLSC b a = runSMT $ unLST (lowerCodensity a) `runGnosticT` freeze b

mapLSC :: Foldable f => f a -> [a]
mapLSC = foldr ( \ a bs -> a : par a bs ) []

concLSC :: [LSC a] -> LSC [a]
concLSC = lift . Par.sequence . fmap lowerCodensity

liftSMT :: Symbolic a -> LSC a
liftSMT = lift . LST . lift

ask :: LSC Technology
ask = lift $ LST Reader.ask


type Path = [Component Layer Integer]

type Ring l a = Component l (Component l a)

type SComponent = Component SLayer SInteger

type SLayer = SInteger

type SPath = [SComponent]

type SRing = Ring SInteger SInteger


data Component l a
  = Rect    { _l :: a, _b :: a, _r :: a, _t :: a }
  | Via     { _l :: a, _b :: a, _r :: a, _t :: a, _z :: [l] }
  | Layered { _l :: a, _b :: a, _r :: a, _t :: a, _z :: [l] }
  deriving (Eq, Show)


makeFieldsNoPrefix ''Component

width, height :: Num a => Component l a -> a
width  p = p ^. r - p ^. l
height p = p ^. t - p ^. b

integrate :: l -> Component l a -> Component l a
integrate layer (Rect x1 y1 x2 y2) = Layered x1 y1 x2 y2 (pure layer)
integrate layer rect = over z (layer :) rect

setLayers :: Foldable f => f l -> Component k a -> Component l a
setLayers layer (Rect    x1 y1 x2 y2)   = Layered x1 y1 x2 y2 (toList layer)
setLayers layer (Via     x1 y1 x2 y2 _) = Via     x1 y1 x2 y2 (toList layer)
setLayers layer (Layered x1 y1 x2 y2 _) = Layered x1 y1 x2 y2 (toList layer)


instance Functor (Component l) where
  fmap f (Rect    x1 y1 x2 y2)       = Rect    (f x1) (f y1) (f x2) (f y2)
  fmap f (Via     x1 y1 x2 y2 layer) = Via     (f x1) (f y1) (f x2) (f y2) layer
  fmap f (Layered x1 y1 x2 y2 layer) = Layered (f x1) (f y1) (f x2) (f y2) layer

instance Foldable (Component l) where
  foldMap f p = foldMap f [p ^. l, p ^. b, p ^. r, p ^. t]

instance Default a => Default (Component l a) where
  def = Rect def def def def


inner, outer :: Ring l a -> Component l a
inner p = Rect (p ^. l . r) (p ^. b . t) (p ^. r . l) (p ^. t . b)
outer p = Rect (p ^. l . l) (p ^. b . b) (p ^. r . r) (p ^. t . t)


makeFieldsNoPrefix ''NetGraph

instance Default NetGraph where
  def = NetGraph mempty def mempty mempty mempty

flattenHierarchy :: NetGraph -> [NetGraph]
flattenHierarchy netlist
  = netlist
  : join [ flattenHierarchy model | model <- toList $ netlist ^. subcells ]


makeFieldsNoPrefix ''AbstractGate

instance Default AbstractGate where
  def = AbstractGate mempty def def mempty


makeFieldsNoPrefix ''Net

instance Eq Net where
  (==) = (==) `on` view identifier

instance Ord Net where
  compare = compare `on` view identifier

instance Semigroup Net where
  Net i ns as <> Net _ os bs = Net i (ns <> os) (unionWith mappend as bs)

instance Monoid Net where
  mempty = Net mempty mempty mempty
  mappend = (<>)


makeFieldsNoPrefix ''Gate

instance Eq Gate where
  (==) = (==) `on` view integer

instance Ord Gate where
  compare = compare `on` view integer

instance Default Gate where
  def = Gate mempty mempty def def mempty def


type Arboresence a = (Net, a, a)

data Circuit2D a = Circuit2D [(Gate, a)] [Arboresence a]
  deriving (Eq, Show)


makeFieldsNoPrefix ''Cell

instance Default Cell where
  def = Cell mempty def def def


makeFieldsNoPrefix ''Pin

instance Eq Pin where
  (==) = (==) `on` view identifier

instance Ord Pin where
  compare = compare `on` view identifier

instance Default Pin where
  def = Pin mempty In def


makeFieldsNoPrefix ''Technology

instance Default Technology where
  def = Technology 1000 1 mempty (1000, 1000) 20000 True


lookupDimensions :: Gate -> Technology -> Maybe (Integer, Integer)
lookupDimensions g tech = view dims <$> lookup (g ^. identifier) (tech ^. stdCells)

lambda :: Technology -> Integer
lambda tech = ceiling $ view scaleFactor tech * view featureSize tech

divideArea :: Foldable f => f a -> Technology -> [Integer]

divideArea xs tech = take n $ x : iterate (join (+)) (tech ^. rowSize)
  where n = ceiling $ sqrt $ fromIntegral $ length xs
        x = tech ^. standardPin . _1 . to (* 2)

debug :: [String] -> LSC ()
debug msg = do
  enabled <- view enableDebug <$> ask
  when enabled $ liftIO $ do
    timestamp <- show . round <$> getPOSIXTime
    errorConcurrent $ unlines [unwords $ timestamp : "-" : msg]
