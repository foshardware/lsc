{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}


module LSC.Types where

import Control.Applicative
import Control.Lens hiding (element)
import Control.Concurrent.MSem (MSem)
import Control.Exception
import Data.Default
import Data.Foldable
import Data.Function (on)
import Data.Map (Map, unionWith, lookup)
import Data.Semigroup
import Data.Hashable
import Data.Text (Text)
import Data.Vector (Vector)

import Data.Aeson (encode, FromJSON, ToJSON)

import Control.MILP
import Control.MILP.Types hiding (literal)

import Control.Monad.Codensity
import Control.Monad.Morph
import Control.Monad.Fail
import Control.Monad.Reader
import Control.Monad.State

import Data.Time.Clock.POSIX
import System.Console.Concurrent

import GHC.Generics
import Prelude hiding (lookup)



data RTL = RTL
  { _identifier  :: Identifier
  , _description :: AbstractGate
  , _subcircuits :: Map Identifier RTL
  } deriving (Generic, Show)


data AbstractGate = AbstractGate [LogicPort] [Expr]
  deriving (Generic, Show)

instance Semigroup AbstractGate where
  AbstractGate ps es <> AbstractGate qs fs = AbstractGate (ps <> qs) (es <> fs)

instance Monoid AbstractGate where
  mempty = AbstractGate mempty mempty
  mappend = (<>)


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
  , _subcells    :: Map Identifier NetGraph
  , _gates       :: Vector Gate
  , _nets        :: Map Identifier Net
  } deriving (Generic, Show)

instance ToJSON NetGraph
instance FromJSON NetGraph

instance Hashable NetGraph where
  hashWithSalt s = hashWithSalt s . encode


type Contact = Pin

data Net = Net
  { _identifier :: Identifier
  , _geometry   :: Path
  , _contacts   :: Map Number [Contact]
  } deriving (Generic, Show)

instance ToJSON Net
instance FromJSON Net

instance Hashable Net where
  hashWithSalt s = hashWithSalt s . encode


type Number = Int

type Identifier = Text

data Gate = Gate
  { _identifier :: Identifier
  , _geometry   :: Path
  , _vdd        :: Pin
  , _gnd        :: Pin
  , _wires      :: Map Identifier Identifier
  , _number     :: Number
  } deriving (Generic, Show)

instance ToJSON Gate
instance FromJSON Gate

instance Hashable Gate where
  hashWithSalt s = hashWithSalt s . encode



data Track = Track
  { _offset :: Integer
  , _steps  :: Int
  , _space  :: Integer
  , _z      :: [Layer]
  } deriving (Generic, Show)

instance ToJSON Track
instance FromJSON Track

instance Hashable Track



data AbstractCell = AbstractCell
  { _geometry  :: Path
  , _routing   :: [Either Track Track]
  , _vdd       :: Pin
  , _gnd       :: Pin
  , _pins      :: Map Identifier Pin
  } deriving (Generic, Show)

instance ToJSON AbstractCell
instance FromJSON AbstractCell

instance Hashable AbstractCell where
  hashWithSalt s = hashWithSalt s . encode


data Cell = Cell
  { _pins       :: Map Identifier Pin
  , _vdd        :: Pin
  , _gnd        :: Pin
  , _dims       :: (Integer, Integer)
  } deriving (Generic, Show)

instance ToJSON Cell
instance FromJSON Cell

instance Hashable Cell where
  hashWithSalt s = hashWithSalt s . encode


data Pin = Pin
  { _identifier :: Identifier
  , _dir        :: Maybe Dir
  , _ports      :: [Port]
  } deriving (Generic, Show)

instance ToJSON Pin
instance FromJSON Pin

instance Hashable Pin


type Port = Component Layer Integer


data Dir = In | Out | InOut
  deriving (Eq, Generic, Show)

instance ToJSON Dir
instance FromJSON Dir

instance Hashable Dir


data Layer
  = AnyLayer
  | Metal1
  | Metal2
  | Metal3
  deriving (Eq, Ord, Enum, Read, Generic, Show)

instance ToJSON Layer
instance FromJSON Layer

instance Hashable Layer


data Technology = Technology
  { _scaleFactor    :: Double
  , _featureSize    :: Double
  , _stdCells       :: Map Text Cell
  , _standardPin    :: (Integer, Integer)
  , _rowSize        :: Integer
  } deriving (Generic, Show)

instance ToJSON Technology
instance FromJSON Technology

instance Hashable Technology where
  hashWithSalt s = hashWithSalt s . encode


type Bootstrap = State Technology

bootstrap :: (Technology -> Technology) -> Bootstrap ()
bootstrap = modify

freeze :: Bootstrap () -> Technology
freeze = flip execState def

thaw :: Technology -> Bootstrap ()
thaw = put

type GnosticT m = ReaderT Technology m
type Gnostic = GnosticT Agnostic

type Agnostic = Identity

technology :: LSC Technology
technology = lift $ LST $ lift ask

runGnosticT :: GnosticT m r -> Technology -> m r
runGnosticT = runReaderT

gnostic :: Bootstrap () -> Gnostic r -> r
gnostic b a = a `runReader` freeze b


data CompilerOpts = CompilerOpts
  { _jogs          :: Int
  , _rowSize       :: Integer
  , _halt          :: Int
  , _enableDebug   :: Bool
  , _enableVisuals :: Bool
  , _iterations    :: Int
  , _cutRatio      :: Int
  , _workers       :: Workers
  }

data Workers
  = Singleton
  | Workers (MSem Int)

type Environment = CompilerOpts

type EnvT m = ReaderT Environment m

environment :: LSC CompilerOpts
environment = lift $ LST ask

runEnvT :: Monad m => EnvT m r -> Environment -> m r
runEnvT = evalEnvT

evalEnvT :: Monad m => EnvT m r -> Environment -> m r
evalEnvT = runReaderT


type LSC = Codensity (LST IO)


choice :: Alternative m => [m a] -> m a
choice = foldr (<|>) empty

liftInteger :: LP a -> LSC a
liftInteger = lift . LST . lift . lift . hoist generalize

satisfyInteger, minimizeInteger, maximizeInteger :: LSC Result
satisfyInteger = lift $ LST $ lift $ lift satLPT
minimizeInteger = lift $ LST $ lift $ lift minimizeIO
maximizeInteger = lift $ LST $ lift $ lift maximizeIO


newtype LST m a = LST { unLST :: EnvT (GnosticT (LPT m)) a }

instance Functor m => Functor (LST m) where
  fmap f (LST a) = LST (fmap f a)

instance Monad m => Applicative (LST m) where
  pure = LST . pure
  LST a <*> LST b = LST (a <*> b)

instance Monad m => Monad (LST m) where
  return = pure
  m >>= k = LST (unLST m >>= unLST . k)

instance MonadIO m => MonadIO (LST m) where
  liftIO = LST . liftIO

instance MonadIO m => MonadFail (LST m) where
  fail = throwLSC . Fail

throwLSC :: MonadIO m => Fail -> m a
throwLSC = liftIO . throwIO

newtype Fail = Fail { unFail :: String }

instance Exception Fail

instance Show Fail where
  show = unFail



data Orientation = N | S | W | E | FN | FS | FW | FE
  deriving (Eq, Generic, Show)

instance FromJSON Orientation
instance ToJSON Orientation


instance Hashable Orientation


instance Default Orientation where
  def = N


instance Semigroup Orientation where

  N <> a = a

  S <> S = N
  S <> E = W
  S <> W = E

  W <> E = N
  W <> W = S

  E <> E = S

  FN <> FN = N
  FN <> FS = S
  FN <> FW = W
  FN <> FE = E

  FN <> S = FS
  FN <> W = FW
  FN <> E = FE


  FS <> FS = N
  FS <> FW = E
  FS <> FE = W

  FS <> S = FN
  FS <> W = FE
  FS <> E = FW


  FW <> FE = N
  FW <> FW = S

  FW <> S = FE
  FW <> W = FS
  FW <> E = FN


  FE <> FE = S

  FE <> S = FW
  FE <> W = FS
  FE <> E = FN


  a <> b = b <> a


instance Monoid Orientation where
  mempty = def
  mappend = (<>)



type Path = [Component Layer Integer]

type Ring l a = Component l (Component l a)


type IComponent = Component ILayer Var

type ILayer = Layer

type IPath = [IComponent]

type IRing = Ring ILayer Var

type INodes = Vector (Gate, IComponent)

type IEdges = Map Identifier (Net, IPath)

type IPins = Map Identifier (Pin, IComponent)



data Component l a
  = Rect    { _l :: a, _b :: a, _r :: a, _t :: a }
  | Via     { _l :: a, _b :: a, _r :: a, _t :: a, _z :: [l] }
  | Layered { _l :: a, _b :: a, _r :: a, _t :: a, _z :: [l], _orientation :: Orientation }
  deriving (Eq, Functor, Foldable, Traversable, Generic, Show)

instance (ToJSON l, ToJSON a) => ToJSON (Component l a)
instance (FromJSON l, FromJSON a) => FromJSON (Component l a)

instance (Hashable l, Hashable a) => Hashable (Component l a)

makeFieldsNoPrefix ''Component



data Line a = Line (a, a) (a, a)
  deriving (Eq, Functor, Foldable, Traversable, Generic, Show)

instance ToJSON a => ToJSON (Line a)
instance FromJSON a => FromJSON (Line a)

instance Hashable a => Hashable (Line a)

makeFieldsNoPrefix ''Line



projectNorth :: Component l a -> Component l a
projectNorth p | FN <- p ^. orientation = projectNorth $ p & orientation .~ N
projectNorth p | FS <- p ^. orientation = projectNorth $ p & orientation .~ S
projectNorth p | FW <- p ^. orientation = projectNorth $ p & orientation .~ W
projectNorth p | FE <- p ^. orientation = projectNorth $ p & orientation .~ E
projectNorth p | W <- p ^. orientation = projectNorth $ p & orientation .~ E
projectNorth p | E <- p ^. orientation = p & t .~ p^.r & r .~ p^.t
projectNorth p = p


width, height :: Num a => Component l a -> a
width  p = p ^. r - p ^. l
height p = p ^. t - p ^. b


integrate :: [l] -> Component k a -> Component l a
integrate    [] (Rect    x1 y1 x2 y2)     = Rect    x1 y1 x2 y2
integrate layer (Layered x1 y1 x2 y2 _ o) = Layered x1 y1 x2 y2 layer o
integrate layer (Rect    x1 y1 x2 y2)     = Layered x1 y1 x2 y2 layer def
integrate layer (Via     x1 y1 x2 y2 _)   = Via     x1 y1 x2 y2 layer


flipComponent :: Component l a -> Component l a
flipComponent p = p &~ do
    r .= p ^. t
    t .= p ^. r


instance Default a => Default (Component l a) where
  def = Rect def def def def


inner, outer :: Ring l a -> Component l a
inner p = Rect (p ^. l . r) (p ^. b . t) (p ^. r . l) (p ^. t . b)
outer p = Rect (p ^. l . l) (p ^. b . b) (p ^. r . r) (p ^. t . t)



makeFieldsNoPrefix ''RTL

instance Default RTL where
  def = RTL mempty mempty mempty


makeFieldsNoPrefix ''LogicPort



makeFieldsNoPrefix ''NetGraph

instance Default NetGraph where
  def = NetGraph mempty def mempty mempty mempty


makeFieldsNoPrefix ''Track


makeFieldsNoPrefix ''AbstractCell

instance Default AbstractCell where
  def = AbstractCell mempty mempty def def mempty


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
  (==) = (==) `on` view number

instance Ord Gate where
  compare = compare `on` view number

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
  def = Pin mempty Nothing def

invert :: Pin -> Pin
invert pin | pin ^. dir == pure  In = pin & dir .~ pure Out
invert pin | pin ^. dir == pure Out = pin & dir .~ pure  In
invert pin = pin


makeFieldsNoPrefix ''CompilerOpts

instance Default CompilerOpts where
  def = CompilerOpts 2 100000 (16 * 1000000) True True 4 40 Singleton


runLSC :: Environment -> Bootstrap () -> LSC a -> IO a
runLSC opts tech
  = evalLP start
  . flip runGnosticT (freeze tech)
  . flip runEnvT opts
  . unLST
  . lowerCodensity

evalLSC :: Environment -> Bootstrap () -> LSC a -> IO a
evalLSC = runLSC


debug :: Foldable f => f String -> LSC ()
debug msg = do
  enabled <- view enableDebug <$> environment
  when enabled $ unless (null msg) $ liftIO $ do
    time <- show . round <$> getPOSIXTime
    errorConcurrent $ unlines $ unwords ["->", time] : toList msg
    flushConcurrentOutput


makeFieldsNoPrefix ''Technology

instance Default Technology where
  def = Technology 1000 1 mempty (1000, 1000) 18000


lookupDims :: Gate -> Technology -> Maybe (Integer, Integer)
lookupDims g tech = view dims <$> lookup (g ^. identifier) (tech ^. stdCells)

lambda :: Technology -> Integer
lambda tech = ceiling $ view scaleFactor tech * view featureSize tech


divideArea :: Foldable f => f a -> LSC [Integer]
divideArea xs = do
  size <- view rowSize <$> environment
  tech <- technology
  let x = tech ^. standardPin . _1 & (* 2)
  pure $ take n $ x : fmap (size*) [1..]
  where n = ceiling $ sqrt $ fromIntegral $ length xs


distinctPairs :: [a] -> [(a, a)]
distinctPairs (x : xs) = fmap (x, ) xs ++ distinctPairs xs
distinctPairs _ = []
