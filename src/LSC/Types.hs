{-# LANGUAGE GADTs, DataKinds #-}

module LSC.Types where

import Data.Map (Map)
import Data.Text (Text)

import Control.Monad.Reader
import Control.Monad.State
import Language.SMTLib2
import Language.SMTLib2.Pipe


data Netlist = Netlist [Gate] [Wire]
  deriving (Eq, Show)

data Wire = Wire 
  { source :: Gate
  , target :: Gate
  , wireIndex :: Index
  }
  deriving (Show)

instance Eq Wire where
  w == v = wireIndex w == wireIndex v

type Index = Int

data Gate = Gate
  { gateWires :: [Text]
  , gateIndex :: Index
  }
  deriving (Show)

instance Eq Gate where
  g == h = gateIndex g == gateIndex h


data Component = Component
  { componentPins :: Map Text Pin
  , componentDimensions :: (Integer, Integer)
  } deriving Show

data Pin = Pin Dir Port
  deriving Show

data Port = Port
  { portLayer :: Text
  , portRects :: [Rectangle]
  } deriving Show

type Rectangle = (Integer, Integer, Integer, Integer)

data Dir = In | Out | InOut
  deriving Show

data Technology = Technology
  { padDimensions :: (Integer, Integer)
  , wireWidth :: Integer
  , scaleFactor :: Double
  , components :: Map Text Component
  } deriving Show

defaultTechnology :: Technology
defaultTechnology = Technology (10^15, 10^15) 1 1 mempty

type BootstrapT m = StateT Technology m
type Bootstrap = State Technology

type GnosticT m = ReaderT Technology m
type Gnostic = Reader Technology

bootstrap :: (Technology -> Technology) -> Bootstrap ()
bootstrap = modify

freeze :: Bootstrap () -> Technology
freeze bootstrapping = execState bootstrapping defaultTechnology


type LSC b = GnosticT (SMT b)

runLSC :: LSC b r -> Bootstrap () -> SMT b r
runLSC a b = a `runReaderT` freeze b

