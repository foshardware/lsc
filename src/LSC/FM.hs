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
import Data.Maybe
import Data.Monoid
import Data.IntSet hiding (filter, findMax, foldl', toList)
import qualified Data.IntSet as Set
import Data.IntMap (IntMap, fromListWith, findMax, unionWith, insertWith, adjust, assocs)
import qualified Data.IntMap as Map
import Data.Ratio
import Data.STRef
import Data.Tuple
import Data.Vector (Vector, unsafeFreeze, unsafeThaw, thaw, (!), generate)
import Data.Vector.Mutable hiding (swap, length, set, move)
import Prelude hiding (replicate, length, read)


type FM s = ReaderT (STRef s Heu) (ST s)

balanceFactor :: Rational
balanceFactor = 1 % 2


data Gain a = Gain (IntMap a) (IntMap IntSet)
  deriving Show

instance Semigroup (Gain a) where
  Gain v m <> Gain u n = Gain (u <> v) (unionWith (<>) m n)

instance Monoid (Gain a) where
  mempty = Gain mempty mempty
  mappend = (<>)


data Move
  = Move Int Int -- Move Gain Cell
  deriving Show


type Partition = P IntSet

newtype P a = P { unP :: (a, a) }

instance Eq a => Eq (P a) where
  P (a, _) == P (b, _) = a == b

instance Semigroup a => Semigroup (P a) where
  P (a, b) <> P (c, d) = P (a <> c, b <> d)

instance Monoid a => Monoid (P a) where
  mempty = P mempty
  mappend = (<>)

instance Show a => Show (P a) where
  show (P (a, b)) = "<"++ show a ++", "++ show b ++">"

move :: Int -> Partition -> Partition
move c (P (a, b)) | member c a = P (delete c a, insert c b)
move c (P (a, b)) = P (insert c a, delete c b)

partitionBalance :: Partition -> Int
partitionBalance (P (a, b)) = abs $ size a - size b


data Heu = Heu
  { _partitioning :: Partition
  , _gains        :: Gain Int
  , _freeCells    :: IntSet
  , _moves        :: [Move]
  , _snapshots    :: [Heu]
  , _iterations   :: Int
  } deriving Show

makeFieldsNoPrefix ''Heu


evalFM :: FM s a -> ST s a
evalFM = fmap fst . runFM

execFM :: FM s a -> ST s [Heu]
execFM = fmap snd . runFM

runFM :: FM s a -> ST s (a, [Heu])
runFM f = do
  r <- newSTRef $ Heu mempty mempty mempty mempty mempty 0
  runReaderT ((,) <$> f <*> value snapshots) r


update :: Simple Setter Heu a -> (a -> a) -> FM s ()
update v f = do
  r <- modifySTRef <$> ask
  lift $ r $ v %~ f

value :: Getter Heu a -> FM s a
value v = view v <$> getState

snapshot :: FM s ()
snapshot = update snapshots . (:) . set snapshots mempty =<< getState

getState :: FM s Heu
getState = lift . readSTRef =<< ask


type NetArray  = Vector IntSet
type CellArray = Vector IntSet

type V = CellArray
type E = NetArray


computeG :: FM s (Int, Heu)
computeG = do
  vs <- zip <$> value moves <*> value snapshots
  hs <- getState
  let (_, g, h) = foldl' accum (0, 0, hs) vs
  pure (g, h)
  where
    accum :: (Int, Int, Heu) -> (Move, Heu) -> (Int, Int, Heu)
    accum (gmax, g, h) (Move gc c, heu)
      | g + gc > gmax
      = (g + gc, g + gc, heu)
    accum (gmax, g, h) (Move gc c, heu)
      | g + gc == gmax
      , partitionBalance (view partitioning heu) < partitionBalance (view partitioning h)
      = (gmax, g + gc, heu)
    accum (gmax, g, h) (Move gc c, _)
      = (gmax, g + gc, h)


fiducciaMattheyses (v, e) = do
  initialPartitioning v
  bipartition (v, e)


bipartition :: (V, E) -> FM s Partition
bipartition (v, e) = do
  initialFreeCells v
  initialGains (v, e)
  update moves $ const mempty
  update iterations succ
  p <- value partitioning
  processCell (v, e)
  (g, h) <- computeG
  update partitioning $ const $ h ^. partitioning
  if g <= 0
    then pure p
    else bipartition (v, e)


processCell :: (V, E) -> FM s ()
processCell (v, e) = do
  ci <- selectBaseCell v
  for_ ci $ \ i -> do
    lockCell i
    updateGains (v, e) i
    processCell (v, e)


lockCell :: Int -> FM s ()
lockCell c = do
  update freeCells $ delete c
  update gains $ removeGain c


moveCell :: Int -> FM s ()
moveCell c = do
  Gain v _ <- value gains
  for_ (v ^? ix c) $ \ g -> update moves (Move g c :)
  update partitioning $ move c
  snapshot


selectBaseCell :: V -> FM s (Maybe Int)
selectBaseCell v = do
  g <- value gains
  h <- getState
  pure $ listToMaybe [ x | x <- elems $ snd $ maxGain g, balanceCriterion h x ]


updateGains :: (V, E) -> Int -> FM s ()
updateGains (v, e) c = do
  p <- value partitioning
  let f = fromBlock p c e
  let t = toBlock p c e
  free <- value freeCells
  moveCell c
  for_ (elems $ v ! c) $ \ n -> do
    when (size (t n) == 0) $ sequence_ $ incrementGain <$> elems (f n `intersection` free)
    when (size (t n) == 1) $ sequence_ $ decrementGain <$> elems (t n `intersection` free)
    when (size (f n) == succ 0) $ sequence_ $ decrementGain <$> elems (t n `intersection` free)
    when (size (f n) == succ 1) $ sequence_ $ incrementGain <$> elems (f n `intersection` free)


maxGain :: Gain a -> (Int, IntSet)
maxGain (Gain v m)
  |(i, s) <- findMax m
  , Set.null s
  = maxGain
  $ Gain v
  $ Map.delete i m
maxGain (Gain _ m) = findMax m


removeGain :: Int -> Gain Int -> Gain Int
removeGain c (Gain u m)
  | Just j <- u ^? ix c
  , Just g <- m ^? ix j 
  , Set.null g
  = Gain u
  . Map.delete j
  $ m
removeGain c (Gain u m)
  | Just j <- u ^? ix c
  = Gain u
  . adjust (delete c) j
  $ m
removeGain _ g = g


modifyGain :: (Int -> Int) -> Int -> Gain Int -> Gain Int
modifyGain f c (Gain u m)
  | Just j <- u ^? ix c
  , Just g <- m ^? ix j
  , Set.null g
  = Gain (adjust f c u)
  . adjust (insert c) (f j)
  . Map.delete j
  $ m
modifyGain f c (Gain u m)
  | Just j <- u ^? ix c
  = Gain (adjust f c u)
  . adjust (insert c) (f j)
  . adjust (delete c) j
  $ m
modifyGain _ _ g = g


incrementGain, decrementGain :: Int -> FM s ()
decrementGain = update gains . modifyGain pred
incrementGain = update gains . modifyGain succ


balanceCriterion :: Heu -> Int -> Bool
balanceCriterion h c
  = div v r - k * smax <= a && a <= div v r + k * smax
  where
    P (p, q) = h ^. partitioning
    a = last $ [succ $ size p] ++ [pred $ size p | member c p]
    v = size p + size q
    k = h ^. freeCells . to size
    r = fromIntegral $ denominator balanceFactor `div` numerator balanceFactor
    smax = h ^. gains . to maxGain . _1


initialFreeCells :: V -> FM s ()
initialFreeCells v = update freeCells $ const $ fromAscList [0 .. length v - 1]


initialPartitioning :: V -> FM s ()
initialPartitioning v = update partitioning $ const $
  if length v < 3000
  then P (fromAscList [x | x <- base v, parity x], fromAscList [x | x <- base v, not $ parity x])
  else P (fromAscList [x | x <- base v, half v x], fromAscList [x | x <- base v, not $ half v x])
  where
    base v = [0 .. length v - 1]
    half v i = i <= div (length v) 2
    parity = even


initialGains :: (V, E) -> FM s ()
initialGains (v, e) = do
  p <- value partitioning
  u <- pure $ Map.fromAscList
    [ (,) i
    $ length [() | n <- elems ns, size (f n) == 1]
    - length [() | n <- elems ns, size (t n) == 0]
    | (i, ns) <- [0 .. ] `zip` toList v
    , let f = fromBlock p i e
    , let t = toBlock p i e
    ]
  update gains $ const
    $ Gain u
    $ fromListWith union [ (x, singleton k) | (k, x) <- assocs u ]



fromBlock, toBlock :: Partition -> Int -> E -> Int -> IntSet
fromBlock (P (a, b)) i e n | member i a = intersection a $ e ! n
fromBlock (P (a, b)) i e n = intersection b $ e ! n
toBlock (P (a, b)) i e n | member i a = intersection b $ e ! n
toBlock (P (a, b)) i e n = intersection a $ e ! n


inputRoutine :: Foldable f => Int -> Int -> f (Int, Int) -> FM s (V, E)
inputRoutine n c xs = do
  nv <- replicate n mempty
  cv <- replicate c mempty
  for_ xs $ \ (x, y) -> do
    modify nv (Set.insert y) x
    modify cv (Set.insert x) y
  (,) <$> unsafeFreeze cv <*> unsafeFreeze nv

