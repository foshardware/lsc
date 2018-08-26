{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module LSC.Types where

import Data.Default
import Data.Function (on)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Vector (Vector)

import Control.Monad.Codensity
import Control.Monad.Parallel (MonadFork(..), MonadParallel(..))
import qualified Control.Monad.Parallel as Par
import Control.Monad.Reader (ReaderT(..), Reader, runReader)
import qualified Control.Monad.Reader as Reader
import Control.Monad.State

import Data.SBV


data Netlist = Netlist
  { modelName  :: Text
  , modelPins  :: ([Text], [Text], [Text]) 
  , subModels  :: Map Text Netlist
  , gateVector :: Vector Gate
  , netVector  :: Vector Net
  } deriving Show

instance Monoid Netlist where
  mempty = Netlist mempty mempty mempty mempty mempty
  net1 `mappend` net2 = Netlist
    (modelName  net1 `mappend` modelName  net2)
    (modelPins  net1 `mappend` modelPins  net2)
    (subModels  net1 `mappend` subModels  net2)
    (gateVector net1 `mappend` gateVector net2)
    (netVector  net1 `mappend` netVector  net2)


data Contact = Contact Gate Identifier Pin
  deriving Show

data Net = Net
  { contacts :: [Contact]
  , netIndex :: Index
  } deriving Show

instance Eq Net where
  w == v = netIndex w == netIndex v

instance Ord Net where
  w `compare` v = netIndex w `compare` netIndex v


type Wire = Text

type Identifier = Text

type Index = Int

data Gate = Gate
  { gateIdent :: Identifier
  , gateWires :: Map Identifier Identifier
  , mapWires  :: Map Identifier Identifier
  , gateIndex :: Index
  } deriving Show

instance Eq Gate where
  g == h = gateIndex g == gateIndex h

instance Ord Gate where
  g `compare` h = gateIndex g `compare` gateIndex h

instance Default Gate where
  def = Gate "default" def def def


newtype GateChar = GateChar { unGateChar :: Gate }

instance Eq GateChar where
  GateChar gate1 == GateChar gate2 = gateIdent gate1 == gateIdent gate2

instance Ord GateChar where
  GateChar gate1 `compare` GateChar gate2 = gateIdent gate1 `compare` gateIdent gate2

instance Show GateChar where
  show (GateChar gate) = show gate


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


data Port = Port
  { portLayer :: Text
  , portRects :: [Rectangle]
  } deriving Show

type Rectangle = (Integer, Integer, Integer, Integer)

data Dir = In | Out | InOut
  deriving (Eq, Show)


data Technology = Technology
  { padDimensions :: (Integer, Integer)
  , wireResolution :: Integer
  , wireWidth :: Integer
  , scaleFactor :: Double
  , components :: Map Text Component
  } deriving Show

instance Default Technology where
  def = Technology (10^6 :: Integer, 10^6 :: Integer) 16 1 1 mempty

lookupDimensions :: Technology -> Gate -> (Integer, Integer)
lookupDimensions tech g
  = maybe (0, 0) id
  $ componentDimensions <$> Map.lookup (gateIdent g) (components tech)

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

concLSC :: [LSC a] -> LSC [a]
concLSC = lift . Par.sequence . fmap lowerCodensity

liftSMT :: Symbolic a -> LSC a
liftSMT = lift . LST . lift

ask :: LSC Technology
ask = lift $ LST Reader.ask


data Circuit2D = Circuit2D [Rectangle] [Path]
  deriving (Eq, Show)

newtype Path = Path [(Integer, Integer)]
  deriving (Eq, Show)

