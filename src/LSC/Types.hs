{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE GADTs, DataKinds #-}

module LSC.Types where

import Data.Function (on)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Vector (Vector)

import Control.Monad.Codensity
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
  { gateIdent :: Text
  , gateWires :: [(Text, Text)]
  , mapWires  :: [(Text, Text)]
  , gateIndex :: Index
  } deriving Show

instance Eq Gate where
  g == h = gateIndex g == gateIndex h

instance Ord Gate where
  g `compare` h = gateIndex g `compare` gateIndex h


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

defaultTechnology :: Technology
defaultTechnology = Technology (10^6 :: Integer, 10^6 :: Integer) 16 1 1 mempty

lookupDimensions :: Technology -> Gate -> (Integer, Integer)
lookupDimensions tech g = maybe (0, 0) id $ componentDimensions <$> Map.lookup (gateIdent g) (components tech)

type BootstrapT m = StateT Technology m
type Bootstrap = State Technology

bootstrap :: (Technology -> Technology) -> Bootstrap ()
bootstrap = modify

freeze :: Bootstrap () -> Technology
freeze bootstrapping = execState bootstrapping defaultTechnology

thaw :: Technology -> Bootstrap ()
thaw = put


type GnosticT m = ReaderT Technology m
type Gnostic = Reader Technology

runGnosticT :: GnosticT m r -> Technology -> m r
runGnosticT = runReaderT

gnostic :: Bootstrap () -> Gnostic r -> r
gnostic b a = a `runReader` freeze b


type LSC = Codensity (GnosticT Symbolic)

runLSC :: Bootstrap () -> LSC a -> IO a
runLSC b a = runSMT $ lowerCodensity a `runGnosticT` freeze b

liftSMT :: Symbolic a -> LSC a
liftSMT = lift . lift

ask :: LSC Technology
ask = lift Reader.ask


data Circuit2D = Circuit2D [Rectangle] [Path]
  deriving (Eq, Show)

newtype Path = Path [(Integer, Integer)]
  deriving (Eq, Show)
