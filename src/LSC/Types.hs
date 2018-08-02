{-# LANGUAGE GADTs, DataKinds, TupleSections #-}

module LSC.Types where

import Control.Monad.Reader
import Language.SMTLib2
import Language.SMTLib2.Pipe


data Netlist = Netlist [Gate] [Wire]

data Wire = Wire 
  { source :: Gate
  , target :: Gate
  , wireIndex :: Index
  }

type Index = Int

data Gate = Gate
  { featureSize :: Integer
  , gateIndex :: Index
  }

instance Eq Gate where
  g == h = gateIndex g == gateIndex h


type LSC b = ReaderT Technology (SMT b)

runLSC = runReaderT
 

data Technology = Technology
  { dimensions :: (Integer, Integer)
  }


