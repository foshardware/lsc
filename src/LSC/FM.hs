{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module LSC.FM where

import Control.Lens
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad.ST
import Data.Foldable
import Data.Maybe
import Data.Monoid
import Data.HashTable.ST.Cuckoo (HashTable)
import Data.HashTable.ST.Cuckoo (mutate, lookup, new, newSized)
import Data.IntSet hiding (filter, null, foldl')
import qualified Data.IntSet as S
import Data.Ratio
import Data.STRef
import Data.Vector (Vector, unsafeFreeze, unsafeThaw, freeze, thaw, generate, (!))
import Data.Vector.Mutable (STVector, read, modify, replicate, unsafeSwap)
import Prelude hiding (replicate, length, read, lookup)
import System.Random.MWC



balanceFactor :: Rational
balanceFactor = 1 % 2


data Gain s a = Gain
  (STRef s IntSet)      -- track existing gains
  (STVector s Int)      -- gains indexed by node
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
  { _gains        :: Gain s Int
  , _freeCells    :: IntSet
  , _moves        :: [(Move, Partition)]
  , _iterations   :: !Int
  }

makeFieldsNoPrefix ''Heu


type FM s = ReaderT (GenST s, STRef s (Heu s)) (ST s)


nonDeterministic :: FM RealWorld a -> IO a
nonDeterministic f = withSystemRandom $ \ r -> stToIO $ runFMWithGen r f

prng :: FM s (GenST s)
prng = fst <$> ask


-- | This function does not reach all possible permutations for lists
--   consisting of more than 966 elements. Any PRNGs possible states
--   are bound by its possible seed values.
--   In the case of MWC the period is 2^8222 which allow for roughly
--   969! different states.
--
-- seed bits: 8222
-- maximum list length: 969
--
--   969! =~ 2^8222
--
-- Monotonicity of  n! / (2^n): 
--
-- desired seed bits: 256909
-- desired list length: 20000
--
--   20000! =~ 2^256909
--
randomPermutation :: Int -> FM s (Vector Int)
randomPermutation n = do
  gen <- prng
  v <- st $ unsafeThaw $ generate n id
  for_ [0 .. n - 2] $ \ i -> unsafeSwap v i =<< uniformR (i, n - 1) gen
  unsafeFreeze v


evalFM :: FM s a -> ST s a
evalFM = runFM

runFM :: FM s a -> ST s a
runFM = runFMWithGen undefined

runFMWithGen :: GenST s -> FM s a -> ST s a
runFMWithGen s f = do
  g <- Gain <$> newSTRef mempty <*> thaw mempty <*> new
  r <- newSTRef $ Heu g mempty mempty 0
  runReaderT f (s, r)


st :: ST s a -> FM s a
st = lift


update :: Simple Setter (Heu s) a -> (a -> a) -> FM s ()
update v f = do
  r <- modifySTRef . snd <$> ask
  st $ r $ v %~ f

value :: Getter (Heu s) a -> FM s a
value v = view v <$> snapshot

snapshot :: FM s (Heu s)
snapshot = st . readSTRef . snd =<< ask


type NetArray  = Vector IntSet
type CellArray = Vector IntSet

type V = CellArray
type E = NetArray


multiLevel :: (V, E) -> FM s Partition
multiLevel = fiducciaMattheyses


fiducciaMattheyses :: (V, E) -> FM s Partition
fiducciaMattheyses (v, e) = do

  bipartition (v, e) $
    if length v < 3000
    then P (fromAscList [x | x <- base, even x]) (fromAscList [x | x <- base, not $ even x])
    else P (fromAscList [x | x <- base, half x]) (fromAscList [x | x <- base, not $ half x])

  where
    base = [0 .. length v - 1]
    half i = i <= div (length v) 2


bipartition :: (V, E) -> Partition -> FM s Partition
bipartition (v, e) p = do

  update freeCells $ const $ fromAscList [0 .. length v - 1]
  update moves $ const mempty

  initialGains (v, e) p
  processCell (v, e) p

  (g, q) <- computeG p

  update iterations succ

  if g <= 0
    then pure p
    else bipartition (v, e) q



computeG :: Partition -> FM s (Int, Partition)
computeG p0 = do
  (_, g, h) <- foldl' accum (0, 0, p0) . reverse <$> value moves
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



processCell :: (V, E) -> Partition -> FM s ()
processCell (v, e) p = do
  ck <- selectBaseCell p
  for_ ck $ \ c -> do
    lockCell c
    q <- moveCell c p
    updateGains c (v, e) p
    processCell (v, e) q


lockCell :: Int -> FM s ()
lockCell c = do
  update freeCells $ delete c
  removeGain c


moveCell :: Int -> Partition -> FM s Partition
moveCell c p = do
  Gain _ u _ <- value gains
  g <- st $ read u c
  let q = move c p
  update moves ((Move g c, q) :)
  pure q


selectBaseCell :: Partition -> FM s (Maybe Int)
selectBaseCell p = do
  h <- value freeCells
  bucket <- maxGain
  case bucket of
    Just (i, xs) -> pure $ balanceCriterion h p i `find` xs
    _ -> pure Nothing


updateGains :: Int -> (V, E) -> Partition -> FM s ()
updateGains c (v, e) p = do

  let f = fromBlock p c e
      t = toBlock p c e

  free <- value freeCells

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
      mb <- st $ lookup m g
      if maybe True null mb
        then fail $ "maxGain: empty bucket for gain " <> show g
        else pure $ (g, ) <$> mb
    _ -> pure Nothing


removeGain :: Int -> FM s ()
removeGain c = do

  Gain gmax u m <- value gains

  st $ do

    j <- read u c

    mg <- lookup m j
    for_ mg $ \ ds -> do

        when (ds == pure c) $ modifySTRef gmax $ delete j
        mutate m j $ (, ()) . fmap (filter (/= c))



modifyGain :: (Int -> Int) -> Int -> FM s ()
modifyGain f c = do

  Gain gmax u m <- value gains

  st $ do

    j <- read u c
    modify u f c

    mg <- lookup m j
    for_ mg $ \ ds -> do

        when (ds == pure c) $ modifySTRef gmax $ delete j
        mutate m j $ (, ()) . fmap (filter (/= c))

        modifySTRef gmax $ insert (f j)
        mutate m (f j) $ (, ()) . pure . maybe [c] (c:)



initialGains :: (V, E) -> Partition -> FM s ()
initialGains (v, e) p = do

  let nodes = flip imap v $ \ i ns ->
        let f = fromBlock p i e
            t = toBlock p i e
         in size (S.filter (\ n -> size (f n) == 1) ns)
          - size (S.filter (\ n -> size (t n) == 0) ns)

  let gmax = foldMap singleton nodes

  initial <- st $ do
      gain <- newSized $ size gmax
      flip imapM_ nodes $ \ k x ->
          mutate gain x $ (, ()) . pure . maybe [k] (k:)
      Gain <$> newSTRef gmax <*> thaw nodes <*> pure gain

  update gains $ const initial



balanceCriterion :: IntSet -> Partition -> Int -> Int -> Bool
balanceCriterion h (P p q) smax c
  = div v r - k * smax <= a && a <= div v r + k * smax
  where
    a = last (succ : [pred | member c p]) (size p)
    v = size p + size q
    k = size h
    r = fromIntegral $ denominator balanceFactor `div` numerator balanceFactor


fromBlock, toBlock :: Partition -> Int -> E -> Int -> IntSet
fromBlock (P a _) i e n | member i a = intersection a $ e ! n
fromBlock (P _ b) _ e n = intersection b $ e ! n
toBlock (P a b) i e n | member i a = intersection b $ e ! n
toBlock (P a _) _ e n = intersection a $ e ! n


inputRoutine :: Foldable f => Int -> Int -> f (Int, Int) -> FM s (V, E)
inputRoutine n c xs = st $ do
  ns <- replicate n mempty
  cs <- replicate c mempty
  for_ xs $ \ (x, y) -> do
    modify ns (insert y) x
    modify cs (insert x) y
  (,) <$> freeze cs <*> freeze ns

