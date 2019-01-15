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

import System.IO


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
  , _wires      :: Map Identifier Identifier
  , _integer    :: Int
  } deriving Show


data AbstractGate = AbstractGate
  { _geometry  :: Path
  , _powerRing :: Path
  , _pins      :: Map Identifier Pin
  } deriving Show

data Cell = Cell
  { _pins       :: Map Identifier Pin
  , _dimensions :: (Integer, Integer)
  } deriving Show

data Pin = Pin
  { _identifier :: Identifier
  , _dir        :: Dir
  , _port       :: Port
  } deriving Show

data Port = Port
  { _layer    :: Layer
  , _geometry :: Path
  } deriving Show


data Dir = In | Out | InOut
  deriving (Eq, Show)

data Layer
  = AnyLayer
  | Metal1 | Metal2 | Metal3
  deriving (Eq, Ord, Enum, Show)

data Technology = Technology
  { _scaleFactor    :: Double
  , _featureSize    :: Double
  , _stdCells       :: Map Text Cell
  , _standardPin    :: (Integer, Integer)
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
    fmap liftIO . liftIO . forkExec . runSMT . runGnosticT m =<< LST Reader.ask

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


newtype Comp z a = Comp [(z, Rect a)]
  deriving (Eq, Show)

type Geometry = Comp Integer Integer

type SGeometry = Comp SInteger SInteger


instance Functor (Comp z) where
  fmap f (Comp xs) = Comp [ (z, fmap f a) | (z, a) <- xs ]

instance Semigroup (Comp z a) where
  Comp a <> Comp b = Comp (a <> b)

instance Monoid (Comp z a) where
  mempty = Comp mempty
  mappend = (<>)


data Rect a = Rect (a, a) (a, a)
  deriving (Eq, Show)

left, bottom, right, top :: Rect a -> a
left   (Rect (a, _) _) = a
bottom (Rect (_, a) _) = a
right  (Rect _ (a, _)) = a
top    (Rect _ (_, a)) = a

width, height :: Num a => Rect a -> a
width  r = right r - left r
height r = top r - bottom r


instance Functor Rect where
  fmap f (Rect (a, b) (c, d)) = Rect (f a, f b) (f c, f d)

instance Foldable Rect where
  foldMap f r = foldMap f [left r, bottom r, right r, top r]

instance Default a => Default (Rect a) where
  def = Rect def def


type Ring a = Rect (Rect a)

inner, outer :: Ring a -> Rect a
inner (Rect (l, b) (r, t)) = Rect (right l, top b) (left r, bottom t)
outer (Rect (l, b) (r, t)) = Rect (left l, bottom b) (right r, top t)

type Rectangle = Rect Integer

type Path = [Rectangle]

type SRect = Rect SInteger

type SPath = [SRect]


makeFieldsNoPrefix ''NetGraph

instance Default NetGraph where
  def = NetGraph mempty def mempty mempty mempty

flattenHierarchy :: NetGraph -> [NetGraph]
flattenHierarchy netlist
  = netlist
  : join [ flattenHierarchy model | model <- toList $ netlist ^. subcells ]


makeFieldsNoPrefix ''AbstractGate

instance Default AbstractGate where
  def = AbstractGate mempty mempty def


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

vdd :: Net
vdd = Net "vdd" mempty mempty


makeFieldsNoPrefix ''Gate

instance Eq Gate where
  (==) = (==) `on` view integer

instance Ord Gate where
  compare = compare `on` view integer

instance Default Gate where
  def = Gate mempty mempty mempty def


type Arboresence a = (Net, a, a)

data Circuit2D a = Circuit2D [(Gate, a)] [Arboresence a]
  deriving (Eq, Show)


makeFieldsNoPrefix ''Cell


makeFieldsNoPrefix ''Pin

instance Eq Pin where
  (==) = (==) `on` view identifier

instance Ord Pin where
  compare = compare `on` view identifier

instance Default Pin where
  def = Pin mempty In def


makeFieldsNoPrefix ''Port

instance Default Port where
  def = Port AnyLayer mempty


makeFieldsNoPrefix ''Technology

instance Default Technology where
  def = Technology 1000 1 mempty (1000, 1000) True


lookupDimensions :: Gate -> Technology -> Maybe (Integer, Integer)
lookupDimensions g tech = view dimensions <$> lookup (g ^. identifier) (tech ^. stdCells)

lambda :: Technology -> Integer
lambda t = ceiling $ view scaleFactor t * view featureSize t

debug :: [String] -> LSC ()
debug msg = do
  enabled <- view enableDebug <$> ask
  when enabled $ liftIO $ do
    timestamp <- show . round <$> getPOSIXTime
    hPutStrLn stderr $ unwords $ timestamp : "-" : msg
