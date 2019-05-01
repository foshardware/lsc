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
import Data.IntSet hiding (filter, findMax, foldl')
import qualified Data.IntSet as Set
import Data.IntMap (IntMap, fromListWith, findMax, unionWith, insertWith, adjust)
import qualified Data.IntMap as Map
import Data.STRef
import Data.Tuple
import Data.Vector (Vector, unsafeFreeze, unsafeThaw, thaw, (!))
import Data.Vector.Mutable hiding (swap, length, set, move)
import Prelude hiding (replicate, length)


type FM s = ReaderT (STRef s Heu) (ST s)

balanceFactor :: Float
balanceFactor = 0.5


data Gain a = Gain (IntMap a) (IntMap IntSet)
  deriving Show

instance Semigroup (Gain a) where
  Gain v m <> Gain u n = Gain (u <> v) (unionWith (<>) m n)

instance Monoid (Gain a) where
  mempty = Gain mempty mempty
  mappend = (<>)

maxGain :: Gain a -> (Int, IntSet)
maxGain (Gain _ m) = findMax m


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

cardinal :: Partition -> Int
cardinal (P (a, b)) = size a + size b 

move :: Int -> Partition -> Partition
move c (P (a, b)) | member c a = P (delete c a, insert c b)
move c (P (a, b)) = P (insert c a, delete c b)


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
      , diff heu < diff h
      = (gmax, g + gc, heu)
    accum (gmax, g, h) (Move gc c, _)
      = (gmax, g + gc, h)


fiduccia (v, e) = do
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
lockCell = update freeCells . delete


moveCell :: Int -> FM s ()
moveCell c = do

  Gain v _ <- value gains
  for_ (v ^? ix c) $ \ g -> update moves (Move g c :)

  update partitioning $ move c

  snapshot


selectBaseCell :: V -> FM s (Maybe Int)
selectBaseCell v = do
  g <- value gains
  p <- value partitioning
  f <- value freeCells
  pure $ listToMaybe $
    [ x | x <- elems $ snd $ maxGain g
    , balanceCriterion p f x $ fst $ maxGain g
    ]


updateGains :: (V, E) -> Int -> FM s ()
updateGains (v, e) c = do
  f <- fromBlock c e
  t <- toBlock c e
  free <- value freeCells
  moveCell c
  for_ (elems $ v ! c) $ \ n -> do
    when (size (t n) == 0) $ sequence_ $ incrementGain <$> elems (f n `intersection` free)
    when (size (t n) == 1) $ sequence_ $ decrementGain <$> elems (t n `intersection` free)
    when (size (f n) == succ 0) $ sequence_ $ decrementGain <$> elems (t n `intersection` free)
    when (size (f n) == succ 1) $ sequence_ $ incrementGain <$> elems (f n `intersection` free)


modifyGain :: (Int -> Int) -> Int -> FM s ()
modifyGain f c = update gains $ \ (Gain v m) -> case v ^? ix c of
    Nothing -> Gain v m
    Just gj -> Gain
      (adjust f c v)
      (insertWith union (f gj) (singleton c) $ adjust (delete c) gj m)

incrementGain, decrementGain :: Int -> FM s ()
decrementGain = modifyGain pred
incrementGain = modifyGain succ


balanceCriterion :: Partition -> IntSet -> Int -> Int -> Bool
balanceCriterion p@(P (a, b)) f c s
   = member c f
  && r * v' - k * smax <= a' && a' <= r * v' + k * smax
  where
    smax = fromIntegral s
    a' = fromIntegral $ if member c a then size a - 1 else size a + 1
    v' = fromIntegral $ cardinal p
    k = fromIntegral $ size f
    r = balanceFactor


diff :: Heu -> Float
diff h = (r * v' + k * smax) - (r * v' - k * smax)
  where
    smax = fromIntegral $ fst $ maxGain $ h ^. gains
    a' = fromIntegral $ h ^. partitioning . to unP . _1 . to size
    v' = fromIntegral $ cardinal $ h ^. partitioning
    k = fromIntegral $ h ^. freeCells . to size
    r = balanceFactor


initialFreeCells :: V -> FM s ()
initialFreeCells v = update freeCells $ const $ fromAscList [0 .. length v - 1]


initialPartitioning :: V -> FM s ()
initialPartitioning v = update partitioning $ const $ P
  ( fromAscList [0 .. div (length v) 2 - 1]
  , fromAscList [div (length v) 2 .. length v - 1]
  )


initialGains :: (V, E) -> FM s ()
initialGains (v, e) = do
  m <- replicate (length v) 0
  p <- value partitioning
  ifor_ v $ \ i c -> do
    f <- fromBlock i e
    t <- toBlock i e
    for_ (elems c) $ \ n -> do
      when (size (f n) == 1) $ modify m succ i
      when (size (t n) == 0) $ modify m pred i
  v <- unsafeFreeze m
  update gains $ const
    $ Gain (ifoldMap Map.singleton v)
    $ fromListWith union [ (v ! x, singleton x) | x <- [0 .. length v - 1] ]



fromBlock, toBlock :: Int -> E -> FM s (Int -> IntSet)
fromBlock i e = do
  P (a, b) <- value partitioning
  if member i a
    then pure $ \ n -> intersection a $ e ! n
    else pure $ \ n -> intersection b $ e ! n
toBlock i e = do
  P (a, b) <- value partitioning
  if member i a
    then pure $ \ n -> intersection b $ e ! n
    else pure $ \ n -> intersection a $ e ! n


inputRoutine :: Foldable f => Int -> Int -> f (Int, Int) -> FM s (V, E)
inputRoutine n c xs = do
  nv <- replicate n mempty
  cv <- replicate c mempty
  for_ xs $ \ (x, y) -> do
    modify nv (Set.insert y) x
    modify cv (Set.insert x) y
  (,) <$> unsafeFreeze cv <*> unsafeFreeze nv

