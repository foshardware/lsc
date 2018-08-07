{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE GADTs, DataKinds #-}

module LSC.Types where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)

import Control.Monad.Reader
import Control.Monad.State

import Data.SBV


data Netlist = Netlist [Gate] [Wire]
  deriving (Eq, Show)

data Wire = Wire 
  { source :: (Gate, Pin)
  , target :: (Gate, Pin)
  , wireIndex :: Index
  }
  deriving (Show)

instance Eq Wire where
  w == v = wireIndex w == wireIndex v

type Index = Int

data Gate = Gate
  { gateIdent :: Text
  , gateWires :: [(Text, Text)]
  , gateIndex :: Index
  }
  deriving (Show)

instance Eq Gate where
  g == h = gateIndex g == gateIndex h


data Component = Component
  { componentPins :: Map Text Pin
  , componentDimensions :: (Integer, Integer)
  } deriving Show

data Pin = Pin
  { pinDir  :: Dir
  , pinPort :: Port
  } deriving Show

data Port = Port
  { portLayer :: Text
  , portRects :: [Rectangle]
  } deriving Show

type Rectangle = (Integer, Integer, Integer, Integer)

data Dir = In | Out | InOut
  deriving (Eq, Show)

data Technology = Technology
  { padDimensions :: (Integer, Integer)
  , wireWidth :: Integer
  , scaleFactor :: Double
  , components :: Map Text Component
  } deriving Show

defaultTechnology :: Technology
defaultTechnology = Technology (10^6 :: Integer, 10^6 :: Integer) 1 1 mempty

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


type LSC = GnosticT Symbolic

runLSC :: Bootstrap () -> LSC SBool -> IO SatResult
runLSC b a = sat $ a `runGnosticT` freeze b


type Circuit2D = [Rectangle]

