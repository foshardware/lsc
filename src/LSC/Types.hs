{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module LSC.Types where

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
  { modelName  :: Text
  , modelGate  :: AbstractGate
  , subModels  :: Map Text NetGraph
  , gateVector :: Vector Gate
  , netMapping :: Map Identifier Net
  } deriving Show

instance Default NetGraph where
  def = NetGraph mempty def mempty mempty mempty


flattenHierarchy :: NetGraph -> [NetGraph]
flattenHierarchy netlist
  = netlist
  : join [ flattenHierarchy model | model <- toList $ subModels netlist ]


type Contact = Pin

data Net = Net
  { netIdent :: Identifier
  , netPaths :: [Path]
  , netPins  :: Map Gate [Contact]
  } deriving Show

instance Eq Net where
  w == v = netIdent w == netIdent v

instance Ord Net where
  w `compare` v = netIdent w `compare` netIdent v

instance Semigroup Net where
  Net i ns as <> Net _ os bs = Net i (ns <> os) (unionWith mappend as bs)

instance Monoid Net where
  mempty = Net mempty mempty mempty
  mappend = (<>)


type Wire = Text

type Identifier = Text

type Index = Int

data Gate = Gate
  { gateIdent :: Identifier
  , gatePath  :: Path
  , gateWires :: Map Identifier Identifier
  , gateIndex :: Index
  } deriving Show

instance Eq Gate where
  g == h = gateIndex g == gateIndex h

instance Ord Gate where
  g `compare` h = gateIndex g `compare` gateIndex h

instance Default Gate where
  def = Gate mempty mempty mempty def


data AbstractGate = AbstractGate
  { abstractGatePath :: Path
  , abstractContacts :: [Contact]
  } deriving Show

instance Default AbstractGate where
  def = AbstractGate mempty mempty


data Component = Component
  { componentPins :: Map Text Pin
  , componentDimensions :: (Integer, Integer)
  } deriving Show

data Pin = Pin
  { pinIdent :: Text
  , pinDir   :: Dir
  , pinPort  :: Port
  } deriving Show

instance Eq Pin where
  p == q = pinIdent p == pinIdent q

instance Ord Pin where
  compare = compare `on` pinIdent

instance Default Pin where
  def = Pin mempty In def


data Port = Port
  { portLayer :: Text
  , portRects :: [Rectangle]
  } deriving Show

instance Default Port where
  def = Port mempty mempty


data Dir = In | Out | InOut
  deriving (Eq, Show)


data Technology = Technology
  { scaleFactor    :: Double
  , featureSize    :: Double
  , components     :: Map Text Component
  , standardPin    :: (Integer, Integer)
  , enableDebug    :: Bool
  } deriving Show

instance Default Technology where
  def = Technology 1000 1 mempty (1000, 1000) True

lambda :: Technology -> Integer
lambda t = ceiling $ scaleFactor t * featureSize t

lookupDimensions :: Gate -> Technology -> Maybe (Integer, Integer)
lookupDimensions g tech = componentDimensions
  <$> lookup (gateIdent g) (components tech)

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

debug :: [String] -> LSC ()
debug msg = do
  enabled <- enableDebug <$> ask
  when enabled $ liftIO $ do
    timestamp <- show . round <$> getPOSIXTime
    hPutStrLn stderr $ unwords $ timestamp : "-" : msg


type Arboresence = (Net, [Rectangle], [Path])

data Circuit2D a = Circuit2D [(Gate, Path)] a
  deriving (Eq, Show)


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


type Ring a = Rect (Rect a)


type Rectangle = Rect Integer

type Path = [Rectangle]

type SRectangle = Rect SInteger

type SPath = [SRectangle]

