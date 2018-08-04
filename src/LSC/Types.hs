{-# LANGUAGE GADTs, DataKinds, TupleSections #-}

module LSC.Types where

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
 

data Technology = Technology
  { dimensions :: (Integer, Integer)
  , wireWidth :: Integer
  }

type BootstrapT m = StateT Technology m

type Bootstrap = State Technology

bootstrap :: (Technology -> Technology) -> Bootstrap ()
bootstrap = modify
