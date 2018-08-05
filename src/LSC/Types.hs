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


type LSC b = ReaderT Technology (SMT b)

runLSC = runReaderT


data Component = Component
  { componentPins :: Map Text Pin
  , componentDimensions :: (Integer, Integer)
  }

data Pin = Pin
  { pinDirection :: Dir
  , pinContacts :: [Rectangle]
  , pinLayer :: Text
  }

type Rectangle = (Double, Double, Double, Double)

data Dir = In | Out | InOut

data Technology = Technology
  { padDimensions :: (Integer, Integer)
  , wireWidth :: Integer
  , components :: Map Text Component
  }

type BootstrapT m = StateT Technology m
type Bootstrap = State Technology

bootstrap :: (Technology -> Technology) -> Bootstrap ()
bootstrap = modify
