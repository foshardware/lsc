-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module LSC.Model where

import Control.Lens
import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.MSem (MSem)
import qualified Control.Concurrent.MSem as MSem
import Control.DeepSeq

import Data.Aeson (FromJSON, ToJSON)
import Data.Bits
import Data.Default
import Data.HashMap.Lazy (HashMap, unionWith)
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.Scientific
import Data.Semigroup
import Data.Text (Text)
import qualified Data.Text.Lazy as Lazy
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int
import Data.Vector (Vector)

import GHC.Generics
import System.IO

import LSC.Cartesian
import LSC.Component
import LSC.Log
import LSC.Polygon



data RTL = RTL
  { _identifier  :: Identifier
  , _description :: AbstractGate
  , _subcircuits :: HashMap Identifier RTL
  } deriving (Generic, Show)

instance Default RTL where
  def = RTL
      { _identifier  = mempty
      , _description = mempty
      , _subcircuits = mempty
      }


data AbstractGate = AbstractGate [LogicPort] [Expr]
  deriving (Generic, Show)

instance Semigroup AbstractGate where
  AbstractGate ps es <> AbstractGate qs fs = AbstractGate (ps <> qs) (es <> fs)

instance Monoid AbstractGate where
  mempty = AbstractGate mempty mempty


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
  , _subcells    :: HashMap Identifier NetGraph
  , _gates       :: Vector Gate
  , _nets        :: HashMap Identifier Net
  } deriving (Generic, NFData, FromJSON, ToJSON, Show)

instance Default NetGraph where
  def = NetGraph
      { _identifier = mempty
      , _supercell  = def
      , _subcells   = mempty
      , _gates      = mempty
      , _nets       = mempty
      }


data Net = Net
  { _identifier  :: Identifier
  , _geometry    :: [Polygon' Layer Int]
  , _netSegments :: [Line' Int]
  , _members     :: [Gate]
  , _contacts    :: HashMap Number [Pin]
  } deriving (Generic, NFData, FromJSON, ToJSON, Show)


instance Semigroup Net where

  Net "" xs ss ns as <> Net is ys ts os bs
    = Net is (xs <> ys) (ss <> ts) (ns <> os) (unionWith (<>) as bs)
  Net is xs ss ns as <> Net  _ ys ts os bs
    = Net is (xs <> ys) (ss <> ts) (ns <> os) (unionWith (<>) as bs)

  stimes = stimesIdempotent


instance Monoid Net where
  mempty = Net "" mempty mempty mempty mempty


type Number = Int

type Identifier = Text

base16Identifier :: Int -> Identifier
base16Identifier
  = head
  . Lazy.toChunks
  . toLazyTextWith (finiteBitSize (0 :: Int) `shiftR` 2)
  . hexadecimal



data Gate = Gate
  { _identifier  :: Identifier
  , _geometry    :: Component' Layer Int
                    -- | Key: Pin, Value: Net
  , _wires       :: HashMap Identifier Identifier
  , _number      :: Number
  , _fixed       :: Bool
  , _feedthrough :: Bool
  } deriving (Eq, Generic, NFData, FromJSON, ToJSON, Show)

instance Default Gate where
  def = Gate
      { _identifier  = mempty
      , _geometry    = rect 0 0 0 0
      , _wires       = mempty
      , _number      = -1
      , _fixed       = False
      , _feedthrough = False
      }


data Track = Track
  { _stabs :: IntSet
  , _z     :: IntSet
  } deriving (Generic, NFData, FromJSON, ToJSON, Show)



data Row = Row
  { _identifier  :: Identifier
  , _number      :: Number 
  , _l           :: Int
  , _b           :: Int
  , _cardinality :: Int
  , _granularity :: Int
  , _transformation :: Orientation
  } deriving (Generic, NFData, FromJSON, ToJSON, Show)

instance Default Row where
    def = Row
        { _identifier  = mempty
        , _number      = -1
        , _l           = 0
        , _b           = 0
        , _cardinality = 0
        , _granularity = 1
        , _transformation = mempty
        }


data AbstractCell = AbstractCell
  { _geometry  :: [Component' Layer Int]
  , _tracks    :: [Either Track Track]
  , _rows      :: IntMap Row
  , _vdd       :: Pin
  , _gnd       :: Pin
  , _pins      :: HashMap Identifier Pin
  , _pstar     :: Maybe Double
  } deriving (Generic, NFData, FromJSON, ToJSON, Show)

instance Default AbstractCell where
  def = AbstractCell
      { _geometry = mempty
      , _tracks   = mempty
      , _rows     = mempty
      , _vdd      = def
      , _gnd      = def
      , _pins     = mempty
      , _pstar    = Nothing
      }


data Cell = Cell
  { _pins       :: HashMap Identifier Pin
  , _vdd        :: Pin
  , _gnd        :: Pin
  , _dims       :: (Int, Int)
  } deriving (Generic, NFData, FromJSON, ToJSON, Show)

instance Default Cell where
  def = Cell
      { _pins = mempty
      , _vdd  = def
      , _gnd  = def
      , _dims = def
      }



type Port = Polygon' Layer Int


data Dir = In | Out | InOut
  deriving (Eq, Generic, NFData, FromJSON, ToJSON, Show)


data Pin = Pin
  { _identifier :: Identifier
  , _dir        :: Maybe Dir
  , _geometry   :: [Port]
  } deriving (Generic, NFData, FromJSON, ToJSON, Show)

instance Default Pin where
  def = Pin
      { _identifier = mempty
      , _dir        = Nothing
      , _geometry   = mempty
      }



data Technology = Technology
  { _scaleFactor    :: Scientific
  , _stdCells       :: HashMap Identifier Cell
  } deriving (Generic, NFData, FromJSON, ToJSON, Show)


instance Default Technology where
  def = Technology
      { _scaleFactor = 1
      , _stdCells    = mempty
      }



type Workers = MSem Word

createWorkers :: Word -> IO (Maybe Workers)
createWorkers 0 = pure Nothing
createWorkers n = Just <$> MSem.new n

rtsWorkers :: IO (Maybe Workers)
rtsWorkers = createWorkers . fromIntegral . max 1 =<< getNumCapabilities


data CompilerOpts = CompilerOpts
  { _rowCapacity   :: Double 
  , _logLevel      :: LogLevel
  , _visuals       :: Bool
  , _iterations    :: Word
  , _workers       :: Maybe Workers
  , _entropy       :: Maybe Handle
  , _pstar         :: Bool
  }

instance Default CompilerOpts where
  def = CompilerOpts
      { _rowCapacity   = 1
      , _logLevel      = Warning
      , _visuals       = False
      , _iterations    = 1
      , _workers       = Nothing
      , _entropy       = Nothing
      , _pstar         = False
      }


makeFieldsNoPrefix ''RTL

makeFieldsNoPrefix ''LogicPort

makeFieldsNoPrefix ''NetGraph

makeFieldsNoPrefix ''Track

makeFieldsNoPrefix ''Row

makeFieldsNoPrefix ''AbstractCell

makeFieldsNoPrefix ''Cell

makeFieldsNoPrefix ''Pin

makeFieldsNoPrefix ''Gate

makeFieldsNoPrefix ''Net

makeFieldsNoPrefix ''CompilerOpts

makeFieldsNoPrefix ''Technology



instance Plated RTL where
  plate f g = set subcircuits <$> views subcircuits (traverse f) g <*> pure g


instance Plated NetGraph where
  plate f g = set subcells <$> views subcells (traverse f) g <*> pure g


