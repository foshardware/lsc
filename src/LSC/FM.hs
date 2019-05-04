{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module LSC.FM where

import Control.Lens
import Control.Monad
import Control.Monad.Reader
import Control.Monad.ST
import Data.Foldable
import Data.Maybe
import Data.Monoid
import Data.HashTable.ST.Cuckoo (HashTable)
import qualified Data.HashTable.ST.Cuckoo as H
import Data.IntSet (IntSet, maxView, intersection, insert, delete, member, size, fromAscList, elems)
import qualified Data.IntSet as S
import Data.Ratio
import Data.STRef
import Data.Vector (Vector, unsafeFreeze, (!))
import Data.Vector.Mutable (modify, replicate)
import Prelude hiding (replicate, length, read)


type FM s = ReaderT (STRef s (Heu s)) (ST s)


balanceFactor :: Rational
balanceFactor = 1 % 2


data Gain s a = Gain
  (STRef s IntSet)      -- track existing gains
  (HashTable s a Int)   -- gains indexed by node
  (HashTable s Int [a]) -- nodes indexed by gain


data Move
  = Move Int Int -- Move Gain Cell
  deriving Show


type Partition = P IntSet

data P a = P !a !a

unP :: P a -> (a, a)
unP (P a b) = (a, b)

instance Eq a => Eq (P a) where
  P a _ == P b _ = a == b

instance Semigroup a => Semigroup (P a) where
  P a b <> P c d = P (a <> c) (b <> d)

instance Monoid a => Monoid (P a) where
  mempty = P mempty mempty
  mappend = (<>)

instance Show a => Show (P a) where
  show (P a b) = "<"++ show a ++", "++ show b ++">"

move :: Int -> Partition -> Partition
move c (P a b) | member c a = P (delete c a) (insert c b)
move c (P a b) = P (insert c a) (delete c b)

partitionBalance :: Partition -> Int
partitionBalance (P a b) = abs $ size a - size b


data Heu s = Heu
  { _partitioning :: Partition
  , _gains        :: Gain s Int
  , _freeCells    :: IntSet
  , _moves        :: [(Move, Partition)]
  , _iterations   :: Int
  }

makeFieldsNoPrefix ''Heu


st :: ST s a -> FM s a
st = lift


evalFM :: FM s a -> ST s a
evalFM = runFM

runFM :: FM s a -> ST s a
runFM f = do
  g <- Gain <$> newSTRef mempty <*> H.new <*> H.new
  r <- newSTRef $ Heu mempty g mempty mempty 0
  runReaderT f r


update :: Simple Setter (Heu s) a -> (a -> a) -> FM s ()
update v f = do
  r <- modifySTRef <$> ask
  st $ r $ v %~ f

value :: Getter (Heu s) a -> FM s a
value v = view v <$> snapshot

snapshot :: FM s (Heu s)
snapshot = st . readSTRef =<< ask


type NetArray  = Vector IntSet
type CellArray = Vector IntSet

type V = CellArray
type E = NetArray


computeG :: FM s (Int, Partition)
computeG = do
  p <- value partitioning
  (_, g, h) <- foldl' accum (0, 0, p) . reverse <$> value moves
  pure (g, h)
  where
    accum :: (Int, Int, Partition) -> (Move, Partition) -> (Int, Int, Partition)
    accum (gmax, g, _) (Move gc _, q)
      | g + gc > gmax
      = (g + gc, g + gc, q)
    accum (gmax, g, p) (Move gc _, q)
      | g + gc == gmax
      , partitionBalance p > partitionBalance q
      = (gmax, g + gc, q)
    accum (gmax, g, p) (Move gc _, _)
      = (gmax, g + gc, p)


fiducciaMattheyses :: (V, E) -> FM s Partition
fiducciaMattheyses (v, e) = do

  update partitioning $ const $
    if length v < 3000
    then P (fromAscList [x | x <- base, even x]) (fromAscList [x | x <- base, not $ even x])
    else P (fromAscList [x | x <- base, half x]) (fromAscList [x | x <- base, not $ half x])

  bipartition (v, e)

  where
    base = [0 .. length v - 1]
    half i = i <= div (length v) 2


bipartition :: (V, E) -> FM s Partition
bipartition (v, e) = do

  update freeCells $ const $ fromAscList [0 .. length v - 1]
  update moves $ const mempty

  initialGains (v, e)
  processCell (v, e)

  (g, p) <- computeG

  update iterations succ
  update partitioning $ const p

  if g <= 0
    then pure p
    else bipartition (v, e)


processCell :: (V, E) -> FM s ()
processCell (v, e) = do
  ci <- selectBaseCell
  for_ ci $ \ i -> do
    lockCell i
    updateGains (v, e) i
    processCell (v, e)


lockCell :: Int -> FM s ()
lockCell c = do
  update freeCells $ delete c
  removeGain c


moveCell :: Int -> FM s ()
moveCell c = do
  Gain _ u _ <- value gains
  v <- st $ H.lookup u c
  for_ v $ \ g -> do
    update partitioning $ move c
    p <- value partitioning
    update moves ((Move g c, p) :)


selectBaseCell :: FM s (Maybe Int)
selectBaseCell = do
  h <- snapshot
  bucket <- maxGain
  case bucket of
    Just (i, xs) -> pure $ balanceCriterion h i `find` xs
    _ -> pure Nothing


updateGains :: (V, E) -> Int -> FM s ()
updateGains (v, e) c = do

  p <- value partitioning

  let f = fromBlock p c e
  let t = toBlock p c e
  free <- value freeCells

  moveCell c

  for_ (elems $ v ! c) $ \ n -> do

    -- reflect changes before the move
    when (size (t n) == 0) $ for_ (elems $ f n `intersection` free) (modifyGain succ)
    when (size (t n) == 1) $ for_ (elems $ t n `intersection` free) (modifyGain pred)

    -- reflect changes after the move
    when (size (f n) == succ 0) $ for_ (elems $ t n `intersection` free) (modifyGain pred)
    when (size (f n) == succ 1) $ for_ (elems $ f n `intersection` free) (modifyGain succ)



maxGain :: FM s (Maybe (Int, [Int]))
maxGain = do
  Gain gmax _ m <- value gains
  mg <- st $ maxView <$> readSTRef gmax
  case mg of
    Just (g, _) -> do
      mb <- st $ H.lookup m g
      if maybe True null mb
        then fail $ "maxGain: empty bucket for gain " <> show g
        else pure $ (g, ) <$> mb
    _ -> pure Nothing


removeGain :: Int -> FM s ()
removeGain c = do

  Gain gmax u m <- value gains

  st $ do

    mj <- H.lookup u c

    for_ mj $ \ j -> do
      mg <- H.lookup m j
      for_ mg $ \ ds -> do

        when (ds == pure c) $ modifySTRef gmax $ delete j
        H.mutate m j $ (, ()) . fmap (filter (/= c))



modifyGain :: (Int -> Int) -> Int -> FM s ()
modifyGain f c = do

  Gain gmax u m <- value gains

  st $ do

    mj <- H.lookup u c
    H.mutate u c $ (, ()) . fmap f

    for_ mj $ \ j -> do
      mg <- H.lookup m j
      for_ mg $ \ ds -> do

        when (ds == pure c) $ modifySTRef gmax $ delete j
        H.mutate m j $ (, ()) . fmap (filter (/= c))

        modifySTRef gmax $ insert (f j)
        H.mutate m (f j) $ (, ()) . pure . maybe [c] (c:)



initialGains :: (V, E) -> FM s ()
initialGains (v, e) = do

  p <- value partitioning

  g <- st $ do

    gmax <- newSTRef mempty

    u <- H.newSized $ length v
    m <- H.new

    flip imapM_ v $ \ i ns -> do
      let f = fromBlock p i e
      let t = toBlock p i e
      H.insert u i
        $ size (S.filter (\ n -> size (f n) == 1) ns)
        - size (S.filter (\ n -> size (t n) == 0) ns)

    flip H.mapM_ u $ \ (k, x) -> do
      modifySTRef gmax $ insert x
      H.mutate m x $ (, ()) . pure . maybe [k] (k:)

    pure $ Gain gmax u m

  update gains $ const g



balanceCriterion :: Heu s -> Int -> Int -> Bool
balanceCriterion h smax c
  = div v r - k * smax <= a && a <= div v r + k * smax
  where
    P p q = h ^. partitioning
    a = last $ [succ $ size p] ++ [pred $ size p | member c p]
    v = size p + size q
    k = h ^. freeCells . to size
    r = fromIntegral $ denominator balanceFactor `div` numerator balanceFactor


fromBlock, toBlock :: Partition -> Int -> E -> Int -> IntSet
fromBlock (P a _) i e n | member i a = intersection a $ e ! n
fromBlock (P _ b) _ e n = intersection b $ e ! n
toBlock (P a b) i e n | member i a = intersection b $ e ! n
toBlock (P a _) _ e n = intersection a $ e ! n


inputRoutine :: Foldable f => Int -> Int -> f (Int, Int) -> FM s (V, E)
inputRoutine n c xs = lift $ do
  nv <- replicate n mempty
  cv <- replicate c mempty
  for_ xs $ \ (x, y) -> do
    modify nv (insert y) x
    modify cv (insert x) y
  (,) <$> unsafeFreeze cv <*> unsafeFreeze nv

