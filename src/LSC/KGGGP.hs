{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module LSC.KGGGP where

import Control.Lens hiding (parts, set)
import Control.Monad
import Control.Monad.Loops
import Control.Monad.Reader
import Control.Monad.ST
import Data.Foldable
import Data.Function
import Data.Maybe
import Data.Monoid
import Data.HashTable.ST.Cuckoo (HashTable)
import Data.HashTable.ST.Cuckoo (mutate, lookup, new)
import Data.IntSet hiding (filter, null, foldr, foldl', toList)
import qualified Data.IntSet as S
import Data.Ratio
import Data.STRef
import Data.Vector
  ( Vector
  , unsafeThaw
  , freeze, thaw
  , replicate
  , (!)
  )
import Data.Vector.Mutable
  ( MVector
  , read, write, modify
  , take
  )
import qualified Data.Vector.Mutable as M
import Prelude hiding (replicate, length, read, lookup, take, drop)



balanceFactor :: Rational
balanceFactor = 1 % 10


type NetArray  = Vector IntSet
type CellArray = Vector IntSet


type V = CellArray
type E = NetArray



data Gain s a = Gain
  String -- description
  (STRef s Int, MVector s IntSet)  -- track existing gains
  (Vector (MVector s Int))         -- gains indexed by node
  (Vector (HashTable s Int [a]))   -- nodes paired with partition indexed by gain


numberOfPartitions :: Gain s Int -> Int
numberOfPartitions (Gain _ _ x _) = length x



type Displacement = (Int, Int)



data Move
  = Move Int Int -- Move Gain Cell
  deriving Show


type Partitioning = Vector IntSet

type MPartitioning s = MVector s IntSet


cutSizes :: V -> Partitioning -> [Int]
cutSizes v partitioning =
    [ size $ foldMap (v!) (elems p) `intersection` foldMap (v!) (elems q)
    | p <- toList partitioning
    , q <- toList partitioning
    ]



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
    r <- Heu <$> thaw (replicate 1000 mempty) <*> newSTRef mempty
    runReaderT f r


st :: ST s a -> KGGGP s a
st = lift



balanceConstraint :: V -> Int -> Int -> KGGGP s Bool
balanceConstraint v c p = do
    partitioning <- view parts <$> ask
    let k = M.length partitioning
        wavg = (fromIntegral (length v) % fromIntegral k) * (1 + balanceFactor)

    wi <- fromIntegral . size . insert c <$> read partitioning p

    pure $ wi <= wavg



connectivityConstraint :: V -> Int -> Int -> KGGGP s Bool
connectivityConstraint v c p = do
    partitioning <- view parts <$> ask
    nodes <- elems <$> read partitioning p
    pure $ not $ S.null $ intersection (v!c) (foldMap (v!) nodes)




kgggp :: (V, E) -> Partitioning -> KGGGP s Partitioning
kgggp (v, e) fixed = do

    let k = length fixed

    partitioning <- view parts <$> ask
    for_ [0 .. k - 1] $ \ q -> do
        write partitioning q $ fixed ! q

    free <- view freeCells <$> ask
    st $ writeSTRef free $ fromDistinctAscList [0 .. length v - 1] \\ foldMap id fixed


    hreg <- st $ initialDisplacements (v, e) "hreg" fixed
    hnbc <- st $ newGains v "hnbc" $ length fixed
    hncc <- st $ newGains v "hncc" $ length fixed

    whileM_ (st $ not . S.null <$> readSTRef free) $ do

      (node, part) <- untilJust $ do

          reg <- st $ not <$> emptyGains hreg
          nbc <- st $ not <$> emptyGains hnbc
          ncc <- st $ not <$> emptyGains hncc

          if reg
          then do

            (node, part) <- st $ maximumGain hreg
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

      st $ do

        modifySTRef free $ delete node
        f <- readSTRef free

        for_ [0 .. length fixed - 1] $ \ q -> do
          _ <- removeGain node q hreg
          _ <- removeGain node q hnbc
          _ <- removeGain node q hncc

          update (v, e) node q f hreg
          update (v, e) node q f hnbc
          update (v, e) node q f hncc

        for_ (elems $ intersection f $ foldMap (e!) $ elems (v!node)) $ \ n ->
            moveDisplacement (n, part) hncc hreg


      modify partitioning (insert node) part


    freeze . take k . view parts =<< ask





emptyGains :: Gain s a -> ST s Bool
emptyGains (Gain _ (len, maxg) _ _) = do
    k <- readSTRef len
    emp <- newSTRef True
    for_ [0 .. k - 1] $ \ i -> do
        modifySTRef emp . (&&) . S.null =<< read maxg i
    readSTRef emp



maximumGain :: Gain s Int -> ST s (Int, Int)
maximumGain (Gain _ (len, maxg) _ buckets) = do
    k <- readSTRef len
    gm <- newSTRef (minBound, minBound)
    for_ [0 .. k - 1] $ \ q -> do
        h <- fmap fst . maxView <$> read maxg q
        modifySTRef gm $ \ (g, p) -> last $ (g, p) : [(fromJust h, q) | maybe False (g <) h]
    (g, p) <- readSTRef gm
    Just cs <- lookup (buckets ! p) g
    pure (head cs, p)



moveDisplacement :: Displacement -> Gain s Int -> Gain s Int -> ST s ()
moveDisplacement (c, p) hf ht = do

   gm <- removeGain c p hf
   for_ gm $ \ g -> insertGain c p g ht




newGains :: V -> String -> Int -> ST s (Gain s Int)
newGains v name k = do

    maxg    <- (,) <$> newSTRef k <*> thaw (replicate k mempty)
    nodes   <- sequence $ replicate k $ thaw $ 0 <$ v
    buckets <- sequence $ replicate k new

    pure $ Gain name maxg nodes buckets



initialDisplacements :: (V, E) -> String -> Partitioning -> ST s (Gain s Int)
initialDisplacements (v, e) name fixed = do

    let free = fromDistinctAscList [0 .. length v - 1] \\ foldMap id fixed

    let zeroes n = n <$ mutate n 0 (const (Just (elems free), ()))

    maxg    <- (,) <$> newSTRef (length fixed) <*> thaw (mempty <$ fixed)
    nodes   <- sequence $ thaw (0 <$ v) <$ fixed
    buckets <- sequence $ (zeroes =<< new) <$ fixed

    let gain = Gain name maxg nodes buckets

    flip imapM_ fixed $ \ i p -> do
      for_ (elems p) $ \ c ->
          update (v, e) c i free gain
      g <- foldMap singleton <$> freeze (nodes ! i)
      modify (snd maxg) (<> g) i


    pure gain 




update :: (V, E) -> Int -> Int -> IntSet -> Gain s Int -> ST s ()
update (v, e) c i free gain = do

  let k = numberOfPartitions gain

  for_ (elems $ intersection free $ foldMap (e!) $ elems (v!c)) $ \ n -> do

    for_ [0 .. pred k] $ \ j -> do

      if i /= j
      then modifyGain n j pred gain
      else modifyGain n j succ gain



removeGain :: Int -> Int -> Gain s Int -> ST s (Maybe Int)
removeGain c i (Gain _ maxg nodes buckets) = do

    g <- read (nodes ! i) c
    b <- foldMap id <$> lookup (buckets ! i) g

    when (b == [c]) $ do
        modify (snd maxg) (delete g) i

    mutate (buckets ! i) g $ (, ()) . fmap (filter (/= c))

    pure $ listToMaybe $ g <$ b



insertGain :: Int -> Int -> Int -> Gain s Int -> ST s ()
insertGain c i g (Gain _ maxg nodes buckets) = do

    write (nodes ! i) c g
    modify (snd maxg) (insert g) i
    mutate (buckets ! i) g $ (, ()) . pure . maybe [c] (c:)

 

modifyGain :: Int -> Int -> (Int -> Int) -> Gain s Int -> ST s ()
modifyGain c i f gain = do

    gm <- removeGain c i gain
    for_ gm $ \ g -> insertGain c i (f g) gain



inputRoutine :: Foldable f => Int -> Int -> f (Int, Int) -> ST s (V, E)
inputRoutine n c xs = do
    ns <- unsafeThaw $ replicate n mempty
    cs <- unsafeThaw $ replicate c mempty
    for_ xs $ \ (x, y) -> do
      modify ns (insert y) x
      modify cs (insert x) y
    (,) <$> freeze cs <*> freeze ns


