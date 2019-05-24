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
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
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
  , replicate
  , (!)
  )
import Data.Vector.Mutable
  ( MVector
  , read, write, modify
  , unsafeSwap
  , take, slice
  , copy, grow
  )
import qualified Data.Vector.Mutable as M
import Prelude hiding (replicate, length, read, lookup, take, drop)




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



type Displacement = (Int, Int)



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


runKGGGP :: KGGGP s a -> ST s a
runKGGGP f = do
    r <- Heu <$> thaw mempty  <*> newSTRef mempty
    runReaderT f r


st :: ST s a -> KGGGP s a
st = lift



balanceConstraint :: V -> Int -> Int -> KGGGP s Bool
balanceConstraint v c p = do
    partitioning <- view parts <$> ask
    let k = M.length partitioning
        wavg = (fromIntegral (length v) / fromIntegral k) * (1 + balanceFactor)

    wi <- fromIntegral . size . insert c <$> read partitioning p

    pure $ wi <= wavg



connectivityConstraint :: V -> Int -> Int -> KGGGP s Bool
connectivityConstraint v c p = do
    partitioning <- view parts <$> ask
    nodes <- elems <$> read partitioning p
    pure $ not $ S.null $ intersection (v!c) (foldMap (v!) nodes)




kgggp :: (V, E) -> Partitioning -> KGGGP s Partitioning
kgggp (v, e) fixed = do

    hreg <- st $ initialDisplacements (v, e) fixed
    hnbc <- st $ newGains v $ length fixed
    hncc <- st $ newGains v $ length fixed

    free <- view freeCells <$> ask

    whileM_ (st $ not . S.null <$> readSTRef free) $ do

      (node, part) <- untilJust $ do

          reg <- st $ not <$> emptyGains hreg
          ncc <- st $ not <$> emptyGains hncc
          nbc <- st $ not <$> emptyGains hnbc

          if reg
          then do

            (node, part) <- st $ maximumGain hreg
            partitioning <- view parts <$> ask
            nodes <- read partitioning part

            balance <- balanceConstraint v node part
            connectivity <- connectivityConstraint v node part

            if S.null nodes
            then pure $ Just (node, part)
            else if not balance
                 then st $ Nothing <$ moveDisplacement (node, part) hreg hnbc
            else if not connectivity
                 then st $ Nothing <$ moveDisplacement (node, part) hreg hncc
            else pure $ Just (node, part)

          else if ncc
          then do

            (node, part) <- st $ maximumGain hncc

            balance <- balanceConstraint v node part

            if not balance
            then st $ Nothing <$ moveDisplacement (node, part) hncc hnbc
            else pure $ Just (node, part)

          else if nbc
          then do

            (node, part) <- st $ maximumGain hnbc
            pure $ Just (node, part)

          else pure Nothing


      st $ modifySTRef free $ delete node

      partitioning <- view parts <$> ask
      modify partitioning (insert node) part

      pure ()


    freeze . view parts =<< ask




initKgggp :: (V, E) -> Partitioning -> KGGGP s ()
initKgggp (v, _) fixed = do

    let k = length fixed

    join $ copy
        <$> (pure . take k =<< flip grow k =<< thaw fixed)
        <*> (pure . take k =<< flip grow k . view parts =<< ask)

    free <- view freeCells <$> ask
    st $ writeSTRef free $ fromDistinctAscList [ 0 .. length v - 1] \\ foldMap id fixed



emptyGains :: Gain s a -> ST s Bool
emptyGains (Gain x s _) = fmap isJust $ runMaybeT $ do
    for_ [0 .. length s - 1] $ \ i -> whenM (S.null <$> read x i) mzero



maximumGain :: Gain s Int -> ST s (Int, Int)
maximumGain (Gain maxg _ buckets) = do
    gm <- newSTRef (minBound, 0)
    for_ [0 .. length buckets - 1] $ \ q -> do
        h <- findMin <$> read maxg q
        modifySTRef gm $ \ (g, p) -> last $ (g, p) : [(h, q) | h > g]
    (g, p) <- readSTRef gm
    c <- maybe undefined head <$> lookup (buckets ! p) g
    pure (c, p)



moveDisplacement :: Displacement -> Gain s Int -> Gain s Int -> ST s ()
moveDisplacement (c, p) hf ht = do

   g <- removeGain c p hf
   insertGain c p g ht




newGains :: V -> Int -> ST s (Gain s Int)
newGains v k = do

    maxg    <- thaw $ replicate k mempty
    nodes   <- sequence $ replicate k $ thaw $ 0 <$ v
    buckets <- sequence $ replicate k new

    pure $ Gain maxg nodes buckets



initialDisplacements :: (V, E) -> Partitioning -> ST s (Gain s Int)
initialDisplacements (v, e) fixed = do

    let free = fromDistinctAscList [0 .. length v - 1] \\ foldMap id fixed

    let zeroes n = n <$ mutate n 0 (const (Just [0 .. length v - 1], ()))

    maxg    <- thaw $ mempty <$ fixed
    nodes   <- sequence $ thaw (0 <$ v) <$ fixed
    buckets <- sequence $ (zeroes =<< new) <$ fixed

    let gain = Gain maxg nodes buckets

    flip imapM_ fixed $ \ i p ->
      for_ (elems p) $ \ c -> do

        _ <- removeGain c i gain
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



removeGain :: Int -> Int -> Gain s Int -> ST s Int
removeGain c i (Gain maxg nodes buckets) = do

    g <- read (nodes ! i) c
    b <- foldMap id <$> lookup (buckets ! i) g

    when (null $ tail b) $ do 
        modify maxg (delete g) i

    mutate (buckets ! i) g $ (, ()) . fmap (filter (/= c))

    pure g



insertGain :: Int -> Int -> Int -> Gain s Int -> ST s ()
insertGain c i g (Gain maxg nodes buckets) = do

    write (nodes ! i) c g
    modify maxg (insert g) i
    mutate (buckets ! i) g $ (, ()) . pure . maybe [c] (c:)

 

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



inputRoutine :: Foldable f => Int -> Int -> f (Int, Int) -> ST s (V, E)
inputRoutine n c xs = do
    ns <- unsafeThaw $ replicate n mempty
    cs <- unsafeThaw $ replicate c mempty
    for_ xs $ \ (x, y) -> do
      modify ns (insert y) x
      modify cs (insert x) y
    (,) <$> freeze cs <*> freeze ns


