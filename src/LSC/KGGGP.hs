{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module LSC.KGGGP where

import Control.Conditional (whenM)
import Control.Lens hiding (parts, set)
import Control.Monad
import Control.Monad.Loops
import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad.ST
import Data.Foldable
import Data.Function
import Data.Maybe
import Data.Monoid
import Data.HashTable.ST.Cuckoo (HashTable)
import Data.HashTable.ST.Cuckoo (mutate, lookup, new, newSized)
import Data.IntSet hiding (filter, null, foldr, foldl', toList)
import qualified Data.IntSet as S
import Data.Ratio
import Data.STRef
import Data.Vector
  ( Vector
  , unsafeFreeze, unsafeThaw
  , freeze, thaw
  , (!), indexed
  )
import Data.Vector.Mutable
  ( MVector
  , read, write, modify
  , replicate
  , unsafeSwap
  , take, slice
  , copy, grow
  )
import Prelude hiding (replicate, length, read, lookup, take, drop, head)
import System.Random.MWC

import LSC.Entropy



matchingRatio :: Rational
matchingRatio = 1 % 3

coarseningThreshold :: Int
coarseningThreshold = 8


balanceFactor :: Rational
balanceFactor = 1 % 10


type NetArray  = Vector IntSet
type CellArray = Vector IntSet

type Area = Vector Int

type V = CellArray
type E = NetArray
type A = Area


data Gain s a = Gain
  (MVector s IntSet)               -- track existing gains
  (Vector (MVector s Int))         -- gains indexed by node
  (Vector (HashTable s Int [a]))   -- nodes paired with partition indexed by gain


numberOfPartitions :: Gain s Int -> Int
numberOfPartitions (Gain _ x _) = length x



data Move
  = Move Int Int -- Move Gain Cell
  deriving Show


type Partitioning = Vector IntSet

type MPartitioning s = MVector s IntSet


type Clustering = Vector IntSet


type Permutation = Vector Int


data Heu s = Heu
  { _parts       :: MPartitioning s
  , _freeCells   :: STRef s IntSet
  }

makeFieldsNoPrefix ''Heu


type KGGGP s = ReaderT (Heu s) (ST s)


st :: ST s a -> KGGGP s a
st = lift



initKgggp :: (V, E) -> Partitioning -> KGGGP s ()
initKgggp (v, _) fixed = do

    let k = length fixed

    join $ copy
        <$> (pure . take k =<< flip grow k =<< thaw fixed)
        <*> (pure . take k =<< flip grow k . view parts =<< ask)

    free <- view freeCells <$> ask
    st $ writeSTRef free $ fromDistinctAscList [ 0 .. length v - 1] \\ foldMap id fixed



kgggp :: (V, E) -> Partitioning -> KGGGP s Partitioning
kgggp (v, e) fixed = do

    hreg <- st $ initialDisplacements (v, e) fixed
    hnbc <- st $ undefined :: KGGGP s (Gain s Int)
    hncc <- st $ undefined :: KGGGP s (Gain s Int)

    whileM_ (pure . not . S.null =<< st . readSTRef . view freeCells =<< ask) $ do

      (node, part) <- untilJust $ do

          reg <- st $ not <$> emptyGains hreg
          ncc <- st $ not <$> emptyGains hncc
          nbc <- st $ not <$> emptyGains hnbc

          if reg
          then do

            (node, part) <- st $ maximumGain hreg
            partitioning <- view parts <$> ask
            nodes <- read partitioning part
            if S.null nodes
            then pure $ Just (node, part)
            else if not $ balanceConstraint node part
                 then moveDisplacement (node, part) hreg hnbc
            else if not $ connectivityConstraint node part
                 then moveDisplacement (node, part) hreg hncc
            else pure $ Just (node, part)

          else if ncc
          then do

            (node, part) <- st $ maximumGain hncc

            if not $ balanceConstraint node part
            then moveDisplacement (node, part) hncc hnbc
            else pure $ Just (node, part)

          else if nbc
          then do

            (node, part) <- st $ maximumGain hnbc
            pure $ Just (node, part)

          else pure Nothing


      free <- view freeCells <$> ask 
      st $ modifySTRef free $ delete node

      partitioning <- view parts <$> ask
      modify partitioning (insert node) part

      pure ()


    freeze . view parts =<< ask




emptyGains :: Gain s a -> ST s Bool
emptyGains (Gain x _ _) = all S.null <$> unsafeFreeze x



maximumGain :: Gain s Int -> ST s (Int, Int)
maximumGain (Gain maxg nodes buckets) = pure (0, 0)



balanceConstraint = undefined


connectivityConstraint = undefined


moveDisplacement = undefined



initialDisplacements :: (V, E) -> Partitioning -> ST s (Gain s Int)
initialDisplacements (v, e) fixed = do

    let free = fromDistinctAscList [ 0 .. length v - 1] \\ foldMap id fixed

    let zeroes n = n <$ mutate n 0 (const (Just [0 .. length v - 1], ()))

    maxg    <- thaw $ mempty <$ fixed
    nodes   <- sequence $ thaw (0 <$ v) <$ fixed
    buckets <- sequence $ (zeroes =<< new) <$ fixed

    let gain = Gain maxg nodes buckets

    flip imapM_ fixed $ \ i p ->
      for_ (elems p) $ \ c -> do

        removeGain c i gain
        update (v, e) c i free gain

    pure gain 




update :: (V, E) -> Int -> Int -> IntSet -> Gain s Int -> ST s ()
update (v, e) c i free gain = do

  let k = numberOfPartitions gain

  for_ (elems $ intersection free $ foldMap (e!) $ elems $ v ! c) $ \ n -> do

    for_ [0 .. pred k] $ \ j -> do

      if i /= j
      then modifyGain n j pred gain
      else modifyGain n j succ gain



removeGain :: Int -> Int -> Gain s Int -> ST s ()
removeGain c i (Gain maxg nodes buckets) = do

    g <- read (nodes ! i) c
    b <- foldMap id <$> lookup (buckets ! i) g

    when (null $ tail b) $ do 
        modify maxg (delete g) i

    mutate (buckets ! i) g $ (, ()) . fmap (filter (/= c))

 

modifyGain :: Int -> Int -> (Int -> Int) -> Gain s Int -> ST s ()
modifyGain c i f (Gain maxg nodes buckets) = do

    g <- read (nodes ! i) c
    b <- foldMap id <$> lookup (buckets ! i) g

    modify (nodes ! i) f c

    modify maxg (insert (f g)) i
    when (null $ tail b) $ do 
        modify maxg (delete g) i

    mutate (buckets ! i) g $ (, ()) . fmap (filter (/= c))
    mutate (buckets ! i) (f g) $ (, ()) . pure . maybe [c] (c:)



