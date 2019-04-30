{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module LSC.FM where

import Control.Lens
import Control.Monad
import Control.Monad.Reader
import Control.Monad.ST
import Data.Foldable
import Data.Monoid
import Data.IntSet (IntSet, member, size, intersection, union, elems, fromAscList, singleton)
import qualified Data.IntSet as Set
import Data.IntMap (IntMap, fromListWith)
import qualified Data.IntSet as Set
import Data.STRef
import Data.Tuple
import Data.Vector hiding (modify, length, filter, update, singleton)
import Data.Vector.Mutable hiding (swap, replicate, length)
import Prelude hiding (replicate, length)


type FM s = ReaderT (STRef s Heu) (ST s)

data Heu = Heu
  { _partitioning :: (IntSet, IntSet)
  , _gains        :: (IntMap [Int], IntMap [Int])
  , _freeCells    :: IntSet
  }

makeFieldsNoPrefix ''Heu

update :: Simple Lens Heu a -> (a -> a) -> FM s ()
update v f = do
  r <- ask
  lift $ modifySTRef r $ \ x -> x & v %~ f

value :: Simple Lens Heu a -> FM s a
value v = view v <$> join (lift . readSTRef <$> ask)


type NetArray  = Vector IntSet
type CellArray = Vector IntSet

type V = CellArray
type E = NetArray


type Parts = (IntSet, IntSet)

type P = Parts


data Buckets = Buckets
  Int -- offset

type B = Buckets


type FreeCellList = IntSet


balanceFactor :: Float
balanceFactor = 0.5



fm :: (V, E) -> FM s ()
fm (v, e) = do
  ci <- selectBaseCell
  for_ ci $ \ i -> do
    updateGains (v, e) i
    lockCell i
    fm (v, e)


lockCell :: Int -> FM s ()
lockCell = undefined


selectBaseCell :: FM s (Maybe Int)
selectBaseCell = undefined


updateGains :: (V, E) -> Int -> FM s ()
updateGains (v, e) c = do
  f <- fromBlock c e
  t <- toBlock c e
  for_ (elems $ v ! fromIntegral c) $ \ n -> do
    when (t n == 0) $ pure ()
    when (t n == 1) $ pure ()
    
    when (f n == 0) $ pure ()
    when (f n == 1) $ pure ()


balanceCriterion :: P -> V -> Int -> Bool
balanceCriterion (a, _) v smax
   = balanceFactor * fromIntegral (length v) - fromIntegral smax <= fromIntegral (size a)
  && balanceFactor * fromIntegral (length v) + fromIntegral smax >= fromIntegral (size a)


initialPartitioning :: Int -> FM s ()
initialPartitioning c = update partitioning $ const
  ( fromAscList $ filter ( < div c 2) [0 .. c - 1]
  , fromAscList $ filter (>= div c 2) [0 .. c - 1]
  )

initialGains :: (V, E) -> FM s ()
initialGains (cs, ns) = do
  m <- new $ length cs
  p <- value partitioning
  ifor_ cs $ \ i c -> do
    f <- fromBlock i ns
    t <- toBlock i ns
    for_ (elems c) $ \ n -> do
      when (f n == 1) $ modify m succ i
      when (t n == 0) $ modify m pred i
  v <- unsafeFreeze m
  update gains $ const
    ( fromListWith (<>) [(v ! x, [x]) | x <- elems $ fst p]
    , fromListWith (<>) [(v ! x, [x]) | x <- elems $ snd p]
    )


fromBlock, toBlock :: Int -> E -> FM s (Int -> Int)
fromBlock i e = do
  (a, b) <- value partitioning
  if member i a
    then pure $ \ n -> size $ intersection a $ e ! n
    else pure $ \ n -> size $ intersection b $ e ! n
toBlock i e = do
  (a, b) <- swap <$> value partitioning
  if member i a
    then pure $ \ n -> size $ intersection a $ e ! n
    else pure $ \ n -> size $ intersection b $ e ! n


inputRoutine :: Foldable f => Int -> Int -> f (Int, Int) -> FM s (V, E)
inputRoutine n c xs = do
  nv <- new n
  cv <- new c
  for_ xs $ \ (x, y) -> do
    modify nv (Set.insert y) x
    modify cv (Set.insert x) y
  (,) <$> unsafeFreeze cv <*> unsafeFreeze nv

