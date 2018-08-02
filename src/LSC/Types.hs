{-# LANGUAGE GADTs, DataKinds, TupleSections #-}

module LSC.Types where

import Control.Monad.Reader
import Language.SMTLib2
import Language.SMTLib2.Pipe


data Netlist = Netlist [Gate] [Wire]

data Wire = Wire 
  { sourceGate :: Index
  , targetGate :: Index
  , wireIndex :: Index
  }

type Index = Int

data Gate = Gate
  { featureSize :: Integer
  , gateIndex :: Index
  }
  deriving Eq


type LSC b = ReaderT Technology (SMT b)

runLSC = runReaderT
 

data Technology = Technology
  { dimensions :: (Integer, Integer)
  }

