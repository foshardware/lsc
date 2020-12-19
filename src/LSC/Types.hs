-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}


module LSC.Types where

import Control.Applicative
import Control.Lens hiding (element)
import Control.Concurrent.MSem (MSem)
import Control.Exception
import Control.Monad.ST
import Control.Monad.ST.Unsafe
import Data.Bits
import Data.Default
import Data.Foldable
import Data.Function (on)
import Data.Hashable
import Data.HashMap.Lazy (HashMap, unionWith, lookup)
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.List (sort, group)
import Data.Semigroup
import Data.Text (Text)
import qualified Data.Text.Lazy as Lazy
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int
import Data.Vector (Vector)

import Data.Aeson (encode, FromJSON, ToJSON)

import Control.Monad.Codensity
import Control.Monad.Morph
import Control.Monad.Fail
import Control.Monad.Reader hiding (fail)
import Control.Monad.State hiding (fail)

import Data.Time.Clock.POSIX
import System.Console.Concurrent
import System.IO.Unsafe

import GHC.Generics
import Prelude hiding (lookup, fail)




data RTL = RTL
  { _identifier  :: Identifier
  , _description :: AbstractGate
  , _subcircuits :: HashMap Identifier RTL
  } deriving (Generic, Show)


data AbstractGate = AbstractGate [LogicPort] [Expr]
  deriving (Generic, Show)

instance Semigroup AbstractGate where
  AbstractGate ps es <> AbstractGate qs fs = AbstractGate (ps <> qs) (es <> fs)

instance Monoid AbstractGate where
  mempty = AbstractGate mempty mempty
  mappend = (<>)


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
  } deriving (Generic, Show)

instance ToJSON NetGraph
instance FromJSON NetGraph

instance Hashable NetGraph where
  hashWithSalt s = hashWithSalt s . encode


type Contact = Pin

data Net = Net
  { _identifier :: Identifier
  , _geometry   :: Path
  , _members    :: Vector Gate
  , _contacts   :: HashMap Number [Contact]
  } deriving (Generic, Show)

instance ToJSON Net
instance FromJSON Net

instance Hashable Net where
  hashWithSalt s = hashWithSalt s . encode


type Number = Int

type Identifier = Text

data Gate = Gate
  { _identifier  :: Identifier
  , _space       :: Component Layer Int
  , _wires       :: HashMap Identifier Identifier
  , _number      :: Number  -- ^ pin     ^ net
  , _fixed       :: Bool
  , _feedthrough :: Bool
  } deriving (Generic, Show)

instance ToJSON Gate
instance FromJSON Gate

instance Hashable Gate


data Track = Track
  { _offset :: Int
  , _steps  :: Int
  , _trackSpace :: Int
  , _z      :: IntSet
  } deriving (Generic, Show)

instance ToJSON Track
instance FromJSON Track

instance Hashable Track where
    hashWithSalt s = hashWithSalt s . encode



data Row = Row
  { _identifier  :: Identifier
  , _number      :: Number 
  , _l           :: Int
  , _b           :: Int
  , _orientation :: Orientation
  , _cardinality :: Int
  , _granularity :: Int
  } deriving (Generic, Show)

instance ToJSON Row
instance FromJSON Row

instance Hashable Row



data AbstractCell = AbstractCell
  { _geometry  :: Path
  , _tracks    :: [Either Track Track]
  , _rows      :: IntMap Row
  , _vdd       :: Pin
  , _gnd       :: Pin
  , _pins      :: HashMap Identifier Pin
  } deriving (Generic, Show)

instance ToJSON AbstractCell
instance FromJSON AbstractCell

instance Hashable AbstractCell where
  hashWithSalt s = hashWithSalt s . encode


data Cell = Cell
  { _pins       :: HashMap Identifier Pin
  , _vdd        :: Pin
  , _gnd        :: Pin
  , _dims       :: (Int, Int)
  } deriving (Generic, Show)

instance ToJSON Cell
instance FromJSON Cell

instance Hashable Cell


data Pin = Pin
  { _identifier :: Identifier
  , _dir        :: Maybe Dir
  , _geometry   :: [Component Layer Int]
  } deriving (Generic, Show)

instance ToJSON Pin
instance FromJSON Pin

instance Hashable Pin


type Port = Component Layer Int


data Dir = In | Out | InOut
  deriving (Eq, Generic, Show)

instance ToJSON Dir
instance FromJSON Dir

instance Hashable Dir


data Layer
  = AnyLayer
  | Metal1
  | Metal2
  | Metal3
  | Metal4
  | Metal5
  | Metal6
  | Metal7
  | Metal8
  | Metal9
  | Metal10
  deriving (Eq, Ord, Enum, Generic, Show)

instance ToJSON Layer
instance FromJSON Layer

instance Hashable Layer


data Technology = Technology
  { _scaleFactor    :: Double
  , _featureSize    :: Double
  , _stdCells       :: HashMap Text Cell
  , _standardPin    :: (Int, Int)
  , _rowSize        :: Int
  } deriving (Generic, Show)

instance ToJSON Technology
instance FromJSON Technology

instance Hashable Technology


type Bootstrap = State Technology

bootstrap :: (Technology -> Technology) -> Bootstrap ()
bootstrap = modify

freeze :: Bootstrap () -> Technology
freeze = flip execState def

thaw :: Technology -> Bootstrap ()
thaw = put

type GnosticT m = ReaderT Technology m
type Gnostic = GnosticT Agnostic

type Agnostic = Identity

technology :: LSC Technology
technology = lift $ LST $ lift ask

runGnosticT :: Technology -> GnosticT m r-> m r
runGnosticT = flip runReaderT

gnostic :: Bootstrap () -> Gnostic r -> r
gnostic b a = a `runReader` freeze b


type Workers = Maybe (MSem Word)

data CompilerOpts = CompilerOpts
  { _jogs          :: Int
  , _rowCapacity   :: Double
  , _halt          :: Int
  , _enableDebug   :: Bool
  , _enableVisuals :: Bool
  , _iterations    :: Word
  , _workers       :: Workers
  }


type Environment = CompilerOpts

type EnvT = ReaderT Environment

environment :: LSC CompilerOpts
environment = lift $ LST ask

runEnvT :: Monad m => Environment -> EnvT m r -> m r
runEnvT = evalEnvT

evalEnvT :: Monad m => Environment -> EnvT m r -> m r
evalEnvT = flip runReaderT


type LSC = Codensity (LST IO)


newtype LST m a = LST { unLST :: EnvT (GnosticT m) a }

instance Functor m => Functor (LST m) where
  fmap f (LST a) = LST (fmap f a)

instance Monad m => Applicative (LST m) where
  pure = LST . pure
  LST a <*> LST b = LST (a <*> b)

instance Monad m => Monad (LST m) where
  return = pure
  m >>= k = LST (unLST m >>= unLST . k)

instance MonadIO m => MonadIO (LST m) where
  liftIO = LST . liftIO

instance MonadIO m => MonadFail (LST m) where
  fail = throwLSC . Fail


throwLSC :: MonadIO m => Fail -> m a
throwLSC = liftIO . throwIO

newtype Fail = Fail String

instance Exception Fail

instance Show Fail where
  show (Fail msg) = msg


assert :: MonadFail m => String -> Bool -> m ()
assert _ True = pure ()
assert msg  _ = fail msg



data Orientation = N | S | W | E | FN | FS | FW | FE
  deriving (Eq, Ord, Generic, Show)

instance FromJSON Orientation
instance ToJSON Orientation


instance Hashable Orientation


instance Semigroup Orientation where

  N <> a = a

  S <> S = N
  S <> E = W
  S <> W = E

  W <> E = N
  W <> W = S

  E <> E = S

  FN <> FN = N
  FN <> FS = S
  FN <> FW = W
  FN <> FE = E

  FN <> S = FS
  FN <> W = FW
  FN <> E = FE


  FS <> FS = N
  FS <> FW = E
  FS <> FE = W

  FS <> S = FN
  FS <> W = FE
  FS <> E = FW


  FW <> FE = N
  FW <> FW = S

  FW <> S = FE
  FW <> W = FS
  FW <> E = FN


  FE <> FE = S

  FE <> S = FW
  FE <> W = FS
  FE <> E = FN


  a <> b = b <> a


instance Monoid Orientation where
  mempty = N
  mappend = (<>)



type Path = [Component Layer Int]

type Ring l a = Component l (Component l a)



data Component l a
  = Rect      { _l :: !a, _b :: !a, _r :: !a, _t :: !a }
  | Component { _l :: !a, _b :: !a, _r :: !a, _t :: !a, _z :: !IntSet, _orientation :: !Orientation }
  deriving (Functor, Foldable, Traversable, Generic, Show)

instance ToJSON a => ToJSON (Component l a)
instance FromJSON a => FromJSON (Component l a)

instance ToJSON a => Hashable (Component l a) where
    hashWithSalt s = hashWithSalt s . encode


makeFieldsNoPrefix ''Component


instance Ord a => Semigroup (Component l a) where
    Rect l1 b1 r1 t1 <> Rect l2 b2 r2 t2
        = Rect (min l1 l2) (min b1 b2) (max r1 r2) (max t1 t2)
    Component l1 b1 r1 t1 ls o <> Rect l2 b2 r2 t2
        = Component (min l1 l2) (min b1 b2) (max r1 r2) (max t1 t2) ls o
    Rect l1 b1 r1 t1 <> Component l2 b2 r2 t2 ls o
        = Component (min l1 l2) (min b1 b2) (max r1 r2) (max t1 t2) ls o
    Component l1 b1 r1 t1 ls1 o1 <> Component l2 b2 r2 t2 ls2 o2
        = Component (min l1 l2) (min b1 b2) (max r1 r2) (max t1 t2) (ls1 <> ls2) (o1 <> o2)


instance (Ord a, Bounded a) => Monoid (Component l a) where
    mempty = Rect maxBound maxBound minBound minBound
    mappend = (<>)


instance Eq a => Eq (Component l a) where
    x == y
        = x ^. l == y ^. l
       && x ^. b == y ^. b
       && x ^. r == y ^. r
       && x ^. t == y ^. t
       && x ^. orientation == y ^. orientation 


instance Ord a => Ord (Component l a) where
    compare x y | x ^. l /= y ^. l = compare (x ^. l) (y ^. l)
    compare x y | x ^. b /= y ^. b = compare (x ^. b) (y ^. b)
    compare x y | x ^. r /= y ^. r = compare (x ^. r) (y ^. r)
    compare x y | x ^. t /= y ^. t = compare (x ^. t) (y ^. t)
    compare x y = compare (x ^. orientation) (y ^. orientation)



data Line a = Line (a, a) (a, a)
  deriving (Eq, Functor, Foldable, Traversable, Generic, Show)

instance ToJSON a => ToJSON (Line a)
instance FromJSON a => FromJSON (Line a)

instance Hashable a => Hashable (Line a)

makeFieldsNoPrefix ''Line



implode :: Integral a => Component l a -> Component l a
implode c = Rect (centerX c) (centerY c) (centerX c) (centerY c)
{-# SPECIALIZE implode :: Component l Int -> Component l Int #-}
{-# INLINABLE implode #-}


center :: Integral a => Component l a -> (a, a)
center = liftA2 (,) centerX centerY
{-# SPECIALIZE center :: Component l Int -> (Int, Int) #-}
{-# INLINABLE center #-}


centerX :: Integral a => Component l a -> a
centerX p = div (p ^. r + p ^. l) 2
{-# SPECIALIZE centerX :: Component l Int -> Int #-}
{-# INLINABLE centerX #-}


centerY :: Integral a => Component l a -> a
centerY p = div (p ^. t + p ^. b) 2
{-# SPECIALIZE centerY :: Component l Int -> Int #-}
{-# INLINABLE centerY #-}


moveX :: Num a => a -> Component l a -> Component l a
moveX x p = p &~ do
    l += x
    r += x
{-# SPECIALIZE moveX :: Int -> Component l Int -> Component l Int #-}
{-# INLINABLE moveX #-}


moveY :: Num a => a -> Component l a -> Component l a
moveY y p = p &~ do
    b += y
    t += y
{-# SPECIALIZE moveY :: Int -> Component l Int -> Component l Int #-}
{-# INLINABLE moveY #-}


relocateL :: Num a => a -> Component l a -> Component l a
relocateL x p = p &~ do
    l .= x
    r += x - p ^. l
{-# SPECIALIZE relocateL :: Int -> Component l Int -> Component l Int #-}
{-# INLINABLE relocateL #-}


relocateR :: Num a => a -> Component l a -> Component l a
relocateR x p = p &~ do
    l += x - p ^. r
    r .= x
{-# SPECIALIZE relocateL :: Int -> Component l Int -> Component l Int #-}
{-# INLINABLE relocateR #-}


relocateB :: Num a => a -> Component l a -> Component l a
relocateB y p = p &~ do
    b .= y
    t += y - p ^. b
{-# SPECIALIZE relocateB :: Int -> Component l Int -> Component l Int #-}
{-# INLINABLE relocateB #-}


relocateX :: Integral a => a -> Component l a -> Component l a
relocateX x p = p &~ do
    l += x - centerX p
    r += x - centerX p
{-# SPECIALIZE relocateL :: Int -> Component l Int -> Component l Int #-}
{-# INLINABLE relocateX #-}


relocateY :: Integral a => a -> Component l a -> Component l a
relocateY y p = p &~ do
    b += y - centerY p
    t += y - centerY p
{-# SPECIALIZE relocateL :: Int -> Component l Int -> Component l Int #-}
{-# INLINABLE relocateY #-}


width, height :: Num a => Component l a -> a
width  p = p ^. r - p ^. l
height p = p ^. t - p ^. b
{-# SPECIALIZE width  :: Component l Int -> Int #-}
{-# SPECIALIZE height :: Component l Int -> Int #-}
{-# INLINABLE width  #-}
{-# INLINABLE height #-}


layers :: (HasZ a IntSet, Enum l) => Lens' a [l]
layers = lens
    (map toEnum . IntSet.toList . view z)
    (flip $ set z . IntSet.fromList . map fromEnum)
{-# SPECIALIZE layers :: Lens' (Component Layer a) [Layer] #-}
{-# SPECIALIZE layers :: Lens' Track [Layer] #-}
{-# INLINABLE layers #-}


component :: Component k a -> Component l a
component (Rect x1 y1 x2 y2) = Component x1 y1 x2 y2 mempty mempty
component (Component x1 y1 x2 y2 ls o) = Component x1 y1 x2 y2 ls o



integrate :: Enum l => [l] -> Component k a -> Component l a
integrate ls = set layers ls . component
{-# SPECIALIZE integrate :: [Layer] -> Component k a -> Component Layer a #-}
{-# INLINABLE integrate #-}


inner, outer :: Ring k a -> Component l a
inner p = Rect (p ^. l . r) (p ^. b . t) (p ^. r . l) (p ^. t . b)
outer p = Rect (p ^. l . l) (p ^. b . b) (p ^. r . r) (p ^. t . t)



makeFieldsNoPrefix ''RTL

instance Default RTL where
  def = RTL mempty mempty mempty


makeFieldsNoPrefix ''LogicPort



makeFieldsNoPrefix ''NetGraph

instance Default NetGraph where
  def = NetGraph mempty def mempty mempty mempty


makeFieldsNoPrefix ''Track

makeFieldsNoPrefix ''Row

instance Default Row where
    def = Row mempty (-1) 0 0 mempty 0 1


makeFieldsNoPrefix ''AbstractCell

instance Default AbstractCell where
  def = AbstractCell mempty mempty mempty def def mempty


makeFieldsNoPrefix ''Net


instance Eq Net where
  (==) = (==) `on` view identifier

instance Ord Net where
  compare = compare `on` view identifier


instance Semigroup Net where
  Net i xs ns as <> Net j ys os bs
      | i /= j
      = Net (i <> j) (xs <> ys) (ns <> os) (unionWith (<>) as bs)
  Net i xs ns as <> Net _ ys os bs
      = Net i (xs <> ys) (ns <> os) (unionWith (<>) as bs)


instance Monoid Net where
  mempty = Net mempty mempty mempty mempty
  mappend = (<>)


makeFieldsNoPrefix ''Gate


gateWidth, gateHeight :: Gate -> Int
gateWidth  = width  . view space
gateHeight = height . view space


instance Eq Gate where
  (==) = (==) `on` view number

instance Ord Gate where
  compare = compare `on` view number

instance Default Gate where
  def = Gate mempty mempty mempty (-1) False False



makeFieldsNoPrefix ''Cell

instance Default Cell where
  def = Cell mempty def def def


makeFieldsNoPrefix ''Pin

instance Eq Pin where
  (==) = (==) `on` view identifier

instance Ord Pin where
  compare = compare `on` view identifier

instance Default Pin where
  def = Pin mempty Nothing def

invert :: Pin -> Pin
invert pin | pin ^. dir == pure  In = pin & dir .~ pure Out
invert pin | pin ^. dir == pure Out = pin & dir .~ pure  In
invert pin = pin


makeFieldsNoPrefix ''CompilerOpts

instance Default CompilerOpts where
  def = CompilerOpts 2 100000 (16 * 1000 * 1000) True True 1 Nothing



class Trace m a where
    trace :: a -> m a

instance Show a => Trace IO a where
    trace m = m <$ errorConcurrent (show m ++ "\n")

instance Show a => Trace (ST s) a where
    trace = unsafeIOToST . trace

instance Show a => Trace LSC a where
    trace = liftIO . trace

instance Show a => Trace ((->) a) a where
    trace _ = unsafePerformIO . trace



runLSC :: Environment -> Bootstrap () -> LSC a -> IO a
runLSC opts tech lsc = do
    a <- runGnosticT (freeze tech) $ runEnvT opts $ unLST $ lowerCodensity lsc
    flushConcurrentOutput
    pure a


evalLSC :: Environment -> Bootstrap () -> LSC a -> IO a
evalLSC = runLSC


debug :: [String] -> LSC ()
debug [msg] = do
  enabled <- view enableDebug <$> environment
  when enabled $ liftIO $ do
    time <- base10String . round <$> getPOSIXTime
    errorConcurrent $ unwords ["->", time, msg]
debug msg = do
  enabled <- view enableDebug <$> environment
  when enabled $ unless (null msg) $ liftIO $ do
    time <- base10String . round <$> getPOSIXTime
    errorConcurrent $ unlines $ unwords ["->", time] : toList msg



base10String :: Int -> String
base10String = Lazy.unpack . toLazyText . decimal


base16Text :: Int -> Text
base16Text n
    | x : _ <- Lazy.toChunks . toLazyTextWith (finiteBitSize n `shiftR` 2) $ hexadecimal n
    = x
base16Text _
    = error "base16Text: no chunks"



makeFieldsNoPrefix ''Technology

instance Default Technology where
  def = Technology 1000 1 mempty (1000, 1000) 18000


lookupDims :: Gate -> Technology -> Maybe (Int, Int)
lookupDims g tech = view dims <$> lookup (g ^. identifier) (tech ^. stdCells)

lambda :: Technology -> Int
lambda tech = ceiling $ view scaleFactor tech * view featureSize tech



distinctPairs :: [a] -> [(a, a)]
distinctPairs (x : xs) = zip (repeat x) xs ++ distinctPairs xs
distinctPairs _ = []


unstableUnique :: Ord a => [a] -> [a]
unstableUnique = map head . group . sort
{-# INLINE unstableUnique #-}


median :: Integral a => [a] -> a
median
    = uncurry div
    . foldl' (\ (a, len) x -> (a + x, len + 1)) (0, 0)
    . medianElements


medianElements :: [a] -> [a]
medianElements zs = go zs zs
    where go (x : _)         (_ : []) = [x]
          go (x : y : _) (_ : _ : []) = [x, y]
          go (_ : xs)    (_ : _ : ys) = go xs ys
          go _ _ = error "medianElements: empty list"



foldlWithIndex' :: Foldable f => (b -> Int -> a -> b) -> b -> f a -> b
foldlWithIndex' f y xs = foldl' (\ g x !i -> f (g (i - 1)) i x) (const y) xs (length xs - 1)
{-# INLINE foldlWithIndex' #-}

