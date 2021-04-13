-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}

module LSC.FM where

import Control.Conditional (whenM)
import Control.Lens hiding (indexed, imap)
import Control.Monad
import Control.Monad.Loops
import Control.Monad.ST
import Data.Foldable
import Data.Function
import Data.Maybe
import qualified Data.HashMap.Lazy as HashMap
import Data.HashTable.ST.Basic (HashTable, mutate, lookup, new, newSized)
import Data.IntSet hiding (filter, null, foldr, foldl', toList)
import qualified Data.IntSet as S
import Data.Monoid
import Data.Semigroup
import Data.Ratio
import Data.STRef
import Data.Vector (Vector, unsafeFreeze, take, (!), indexed, imap, unstablePartition)
import qualified Data.Vector as Vector
import qualified Data.Vector.Algorithms.Intro as Intro
import Data.Vector.Mutable (MVector, read, write, modify, replicate, slice)
import Prelude hiding (replicate, length, read, lookup, take, drop, head, unzip)

import LSC.Entropy
import LSC.Model
import LSC.Transformer



matchingRatio :: Rational
matchingRatio = 1 % 3

coarseningThreshold :: Int
coarseningThreshold = 8


balanceFactor :: Rational
balanceFactor = 1 % 10


type NetArray  = Vector IntSet
type CellArray = Vector IntSet


type V = CellArray
type E = NetArray



data Gain s a = Gain
  (STRef s IntSet)      -- track existing gains
  (MVector s Int)       -- gains indexed by node
  (HashTable s Int [a]) -- nodes indexed by gain



data Move
  = Move Int Int -- Move Gain Cell
  deriving Show



type Clustering = Vector IntSet


data Bipartitioning = Bisect !IntSet !IntSet
  deriving Eq


instance Semigroup Bipartitioning where
  Bisect a b <> Bisect c d = Bisect (a <> c) (b <> d)
  stimes = stimesIdempotent

instance Monoid Bipartitioning where
  mempty = Bisect mempty mempty


instance Show Bipartitioning where
  show (Bisect a b) = "<"++ show (elems a) ++", "++ show (elems b) ++">"



type FM s = Cursor s (Heuristic s)

runFM :: FM s a -> Gen s -> ST s (a, Heuristic s)
runFM f g = do
  s <- Gain <$> newSTRef mempty <*> Vector.thaw mempty <*> new
  runCursor f (Heuristic mempty mempty s g)


evalFM :: FM s a -> Gen s -> ST s a
evalFM f g = fst <$> runFM f g



data Heuristic s = Heuristic
  { _unlocked :: IntSet
  , _moves    :: [(Move, Bipartitioning)]
  , _gains    :: Gain s Int
  , _prng     :: Gen s
  }

makeFieldsNoPrefix ''Heuristic



perturbation :: Int -> FM s Permutation
perturbation n = cursor . randomPermutation n <=< position $ view prng


move :: Int -> Bipartitioning -> Bipartitioning
move c (Bisect a b)
  | member c a
  = Bisect (delete c a) (insert c b)
move c (Bisect a b)
  = Bisect (insert c a) (delete c b)


bisectBalance :: Bipartitioning -> Int
bisectBalance (Bisect p q) = abs $ size p - size q

bisectSwap :: Bipartitioning -> Bipartitioning
bisectSwap (Bisect p q) = Bisect q p


cutSize :: (V, E) -> Bipartitioning -> Int
cutSize (v, _) (Bisect p q)
  = size
  $ intersection (foldMap (v!) $ elems p) (foldMap (v!) $ elems q)





fmMultiLevel :: (V, E) -> Int -> Rational -> FM s Bipartitioning
fmMultiLevel (v, _) _ _
  | null v = pure mempty
fmMultiLevel (v, _) _ _
  | 1 <- length v = pure $ Bisect (singleton 0) mempty
fmMultiLevel (v, _) _ _
  | 2 <- length v = pure $ Bisect (singleton 0) (singleton 1)
fmMultiLevel (v, e) t r
  = do

    i <- cursor $ newSTRef 0

    let it = 64

    hypergraphs  <- replicate it mempty
    clusterings  <- replicate it mempty
    partitioning <- replicate it mempty

    write hypergraphs 0 (v, e)

    let continue = cursor $ do
            j <- readSTRef i
            l <- length . fst <$> read hypergraphs j
            s <- Vector.freeze $ slice (max 0 $ j-8) (min 8 $ it-j) hypergraphs
            pure $ j <= it && t <= l && any (l /=) (length . fst <$> s)
    whileM_ continue $ do

      hi <- cursor $ read hypergraphs =<< readSTRef i
      u <- perturbation $ length $ fst hi

      cursor $ do

        modifySTRef i succ

        -- interim clustering
        pk <- match hi r u

        -- interim hypergraph
        hs <- induce hi pk

        j <- readSTRef i
        write clusterings j pk
        write hypergraphs j hs

    -- number of levels
    m <- cursor $ readSTRef i

    by <- read hypergraphs m
    write partitioning m =<< bipartition by =<< bipartitionRandom by

    for_ (reverse [0 .. pred m]) $ \ j -> do
        pk <- read clusterings  $ succ j
        p  <- read partitioning $ succ j

        h <- read hypergraphs j
        q <- rebalance $ project pk p

        write partitioning j =<< bipartition h q

    read partitioning 0




refit :: (V, E) -> Int -> Bipartitioning -> ST s Bipartitioning
refit _ k (Bisect p q)
  | size p <= k
  , size q <= k
  = pure $ Bisect p q
refit _ k (Bisect p q)
  | size p + size q > 2 * k
  = fail $ "refit: impossible size: " ++ show (2 * k, size p + size q)
refit _ _ (Bisect p q)
  | size p == size q
  = pure $ Bisect p q
refit (v, e) k (Bisect p q)
  | size p < size q
  = bisectSwap <$> refit (v, e) k (bisectSwap $ Bisect p q)
refit (v, e) k (Bisect p q)
  = do

    u <- Vector.thaw $ fst $ unstablePartition (\ (i, _) -> member i p) $ imap (,) v

    Intro.partialSortBy (compare `on` \ (_, x) -> criterion p x - criterion q x) u overhang
    s <- fromList . fmap fst . toList . take overhang <$> unsafeFreeze u

    pure $ Bisect (p \\ s) (q <> s)

  where

    criterion x = size . intersection x . foldMap (e!) . elems
    overhang = size p - k



rebalance :: Bipartitioning -> FM s Bipartitioning
rebalance (Bisect p q)
  | size p < size q
  = bisectSwap <$> rebalance (bisectSwap $ Bisect p q)
rebalance (Bisect p q)
  | balanceCriterion (Bisect p q) minBound
  = pure (Bisect p q)
rebalance (Bisect p q)
  = do
    u <- perturbation $ size p + size q
    cursor
      $ do
        b <- newSTRef $ Bisect p q
        i <- newSTRef 0
        let imba c j = j < size p + size q && not (balanceCriterion c (u!j))
        whileM_ (imba <$> readSTRef b <*> readSTRef i)
          $ do
            j <- (u!) <$> readSTRef i
            modifySTRef' i succ
            when (member j p)
              $ modifySTRef b $ move j
        readSTRef b



induce :: (V, E) -> Clustering -> ST s (V, E)
induce (v, e) pk = inputRoutine (length e) (length pk)
  [ (j, k)
  | (k, cluster) <- toList $ indexed pk
  , i <- elems cluster, j <- elems $ v!i
  ]


project :: Clustering -> Bipartitioning -> Bipartitioning
project pk (Bisect p q) = foldMap (pk!) (elems p) `Bisect` foldMap (pk!) (elems q)



match :: (V, E) -> Rational -> Permutation -> ST s Clustering
match (v, e) r u = do

  clustering <- replicate (length v) mempty

  nMatch <- newSTRef 0
  k <- newSTRef 0
  j <- newSTRef 0

  connectivity <- replicate (length v) 0

  sights <- replicate (length v) False
  let yet n = not <$> read sights n
      matched n = write sights n True

  let continue n i = i < length v && n % fromIntegral (length v) < r
  whileM_ (continue <$> readSTRef nMatch <*> readSTRef j)
    $ do

      uj <- (u!) <$> readSTRef j
      whenM (yet uj) $ do

          modify clustering (insert uj) =<< readSTRef k
          matched uj

          let neighbours = elems $ foldMap (e!) (elems $ v!uj)

          for_ neighbours $ \ w -> whenM (yet w)
            $ write connectivity w $ conn w uj

          -- find maximum connectivity
          suchaw <- newSTRef (0, Nothing)
          for_ neighbours $ \ w -> do
              cmax <- fst <$> readSTRef suchaw
              next <- read connectivity w
              when (next > cmax) $ writeSTRef suchaw (next, pure w)

          exists <- snd <$> readSTRef suchaw

          for_ exists $ \ w -> do
              modify clustering (insert w) =<< readSTRef k
              matched w
              modifySTRef' nMatch (+2)

          -- reset connectivity
          for_ neighbours $ modify connectivity (const 0)

          modifySTRef' k succ

      modifySTRef' j succ

  whileM_ ((< length v) <$> readSTRef j)
    $ do

      uj <- (u!) <$> readSTRef j
      whenM (yet uj)
        $ do
          modify clustering (insert uj) =<< readSTRef k
          matched uj
          modifySTRef' k succ

      modifySTRef' j succ

  take <$> readSTRef k <*> unsafeFreeze clustering

  where

      -- cost centre!
      conn i j = sum [ 1 % size (e!x) | x <- elems $ intersection (v'!i) (v'!j) ]
      v' = S.filter (\x -> size (e!x) <= 10) <$> v




bipartitionEven :: (V, E) -> Bipartitioning
bipartitionEven (v, _) = Bisect (S.filter even base) (S.filter odd base)
  where base = fromDistinctAscList [0 .. length v - 1]


bipartitionRandom :: (V, E) -> FM s Bipartitioning
bipartitionRandom (v, _) = do
  u <- perturbation $ length v
  let (p, q) = splitAt (length v `div` 2) (toList u)
  pure $ Bisect (fromList p) (fromList q)



bipartition :: (V, E) -> Bipartitioning -> FM s Bipartitioning
bipartition (v, e) p = do

  hover $ unlocked .~ fromAscList [0 .. length v - 1]
  hover $ moves .~ mempty

  initialGains (v, e) p
  processCell (v, e) p

  (g, q) <- computeG p . reverse <$> position (view moves)

  if g <= 0
    then pure p
    else bipartition (v, e) q



computeG :: Bipartitioning -> [(Move, Bipartitioning)] -> (Int, Bipartitioning)
computeG p0 ms = let (_, g, h) = foldl' accum (0, 0, p0) ms in (g, h)
  where
    accum :: (Int, Int, Bipartitioning) -> (Move, Bipartitioning) -> (Int, Int, Bipartitioning)
    accum (gmax, g, _) (Move gc _, q)
      | g + gc > gmax
      = (g + gc, g + gc, q)
    accum (gmax, g, p) (Move gc _, q)
      | g + gc == gmax
      , bisectBalance p > bisectBalance q
      = (gmax, g + gc, q)
    accum (gmax, g, p) (Move gc _, _)
      = (gmax, g + gc, p)



processCell :: (V, E) -> Bipartitioning -> FM s ()
processCell (v, e) p = do
  ck <- selectBaseCell p
  for_ ck $ \ c -> do
    lockCell c
    q <- moveCell c p
    updateGains c (v, e) p
    processCell (v, e) q


lockCell :: Int -> FM s ()
lockCell c = do
  hover $ unlocked %~ delete c
  removeGain c


moveCell :: Int -> Bipartitioning -> FM s Bipartitioning
moveCell c p = do
  Gain _ u _ <- position $ view gains
  g <- cursor $ read u c
  let q = move c p
  hover $ moves %~ cons (Move g c, q)
  pure q



selectBaseCell :: Bipartitioning -> FM s (Maybe Int)
selectBaseCell p = do
  bucket <- maxGain
  case bucket of
    Just (g, []) -> error $ "selectBaseCell: empty bucket at node " ++ show g
    Just (_, xs) -> pure $ balanceCriterion p `find` xs
    _ -> pure Nothing



updateGains :: Int -> (V, E) -> Bipartitioning -> FM s ()
updateGains c (v, e) p = do

  let f = fromBlock p c e
      t = toBlock p c e

  free <- position $ view unlocked

  for_ (elems $ v ! c) $ \ n -> do

    -- reflect changes before the move
    when (size (t n) == 0) $ for_ (elems $ f n `intersection` free) (modifyGain succ)
    when (size (t n) == 1) $ for_ (elems $ t n `intersection` free) (modifyGain pred)

    -- reflect changes after the move
    when (size (f n) == succ 0) $ for_ (elems $ t n `intersection` free) (modifyGain pred)
    when (size (f n) == succ 1) $ for_ (elems $ f n `intersection` free) (modifyGain succ)



maxGain :: FM s (Maybe (Int, [Int]))
maxGain = do
  Gain gmax _ m <- position $ view gains
  mg <- cursor $ maxView <$> readSTRef gmax
  case mg of
    Just (g, _) -> do
      mb <- cursor $ lookup m g
      pure $ (g, ) <$> mb
    _ -> pure Nothing


removeGain :: Int -> FM s ()
removeGain c = do

  Gain gmax u m <- position $ view gains

  cursor $ do

    j <- read u c

    mg <- lookup m j
    for_ mg $ \ ds -> do

        when (ds == pure c) $ modifySTRef gmax $ delete j
        mutate m j $ (, ()) . fmap (filter (/= c))



modifyGain :: (Int -> Int) -> Int -> FM s ()
modifyGain f c = do

  Gain gmax u m <- position $ view gains

  cursor $ do

    j <- read u c
    modify u f c

    mg <- lookup m j
    for_ mg $ \ ds -> do

        when (ds == pure c) $ modifySTRef gmax $ delete j
        mutate m j $ (, ()) . fmap (filter (/= c))

        modifySTRef gmax $ insert (f j)
        mutate m (f j) $ (, ()) . pure . maybe [c] (c:)



initialGains :: (V, E) -> Bipartitioning -> FM s ()
initialGains (v, e) p = do

  free <- position $ view unlocked

  let nodes = flip imap v $ \ i ns ->
        let f = fromBlock p i e
            t = toBlock p i e
         in size (S.filter (\ n -> size (f n) == 1) ns)
          - size (S.filter (\ n -> size (t n) == 0) ns)

  let gmax = foldMap singleton nodes

  initial <- cursor $ do
      gain <- newSized $ 2 * size gmax + 1
      ifor_ nodes $ \ k x ->
          mutate gain x $ if member x free
             then (, ()) . pure . maybe [k] (k:)
             else (, ())
      Gain <$> newSTRef gmax <*> Vector.thaw nodes <*> pure gain

  hover $ gains .~ initial



balanceCriterion :: Bipartitioning -> Int -> Bool
balanceCriterion (Bisect p q) c
  | size p > size q
  = balanceCriterion (Bisect q p) c
balanceCriterion (Bisect p q) c =
     (fromIntegral (size p + size q) * (1 - balanceFactor)) / 2 <= fromIntegral a
  && (fromIntegral (size p + size q) * (1 + balanceFactor)) / 2 >= fromIntegral b
  where
    a = last (succ : [pred | member c p]) (size p)
    b = last (succ : [pred | member c q]) (size q)


fromBlock, toBlock :: Bipartitioning -> Int -> E -> Int -> IntSet
fromBlock (Bisect a _) i e n
  | member i a
  = intersection a $ e ! n
fromBlock (Bisect _ b) _ e n
  = intersection b $ e ! n
toBlock (Bisect a b) i e n
  | member i a
  = intersection b $ e ! n
toBlock (Bisect a _) _ e n
  = intersection a $ e ! n


inputRoutine :: Foldable f => Int -> Int -> f (Int, Int) -> ST s (V, E)
inputRoutine n c xs = do
  ns <- replicate n mempty
  cs <- replicate c mempty
  for_ xs $ \ (x, y) -> do
    modify ns (insert y) x
    modify cs (insert x) y
  (,) <$> unsafeFreeze cs <*> unsafeFreeze ns
{-# INLINABLE inputRoutine #-}


hypergraph :: NetGraph -> ST s (V, E)
hypergraph top = inputRoutine (views nets length top) (views gates length top)
  [ (n, c)
  | (n, w) <- [0 ..] `zip` views nets toList top
  , c <- views contacts HashMap.keys w
  ]

