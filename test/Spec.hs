{-# LANGUAGE TemplateHaskell #-}

import Control.Category
import Control.Arrow
import Control.Arrow.Select
import Control.Concurrent
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.ST
import Data.Bits
import Data.Default
import Data.FileEmbed
import Data.Foldable
import Data.IntSet (fromAscList, size)
import Data.Map (assocs)
import Data.Ratio
import Data.Text (Text)
import Data.Text.Encoding
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as V
import Prelude hiding (id, (.))

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import LSC
import LSC.BLIF
import LSC.Types
import LSC.FM


main :: IO ()
main = defaultMain $ testGroup "LSC"
  [ fm
  , concurrency
  ]


fm :: TestTree
fm = testGroup "FM" $
  [ testCase "Input routine" $ fmInputRoutine
  , testCase "Deterministic FM" $ fmDeterministic
  , fmML
  ]


fmML :: TestTree
fmML = testGroup "FM Multi Level"
  [ testCase "Random permutation" fmRandomPermutation
  , testCase "Match" fmMatch
  , testCase "Induce and Project" fmInduceAndProject
  , fmRealWorld
  ] 

fmRealWorld :: TestTree
fmRealWorld = testGroup "Real World Instances"
  [ testCase "queue_1.blif"   $ fmMulti (7, 7) =<< stToIO queue_1Hypergraph
  , testCase "picorv32.blif"  $ fmMulti (-1,-1) =<< stToIO picorv32Hypergraph
  ]



fmMulti :: (Int, Int) -> (V, E) -> IO ()
fmMulti (x, y) h = do
  d@(Bisect p q) <- nonDeterministic $ fmMultiLevel h 35 (1%4)
  let c = cutSize h d
  let it = unlines
        [ "cut size in between " ++ show x ++ " and " ++ show y ++ ": " ++ show c
        , show (size p) ++ " | " ++ show (size q)
        ]
  assertBool it $ x <= c && c <= y



fmInduceAndProject :: IO ()
fmInduceAndProject = do
  (v, e) <- arbitraryHypergraph 10000
  nonDeterministic $ do
      u <- randomPermutation $ length v
      c <- st $ match (v, e) matchingRatio u
      h <- st $ induce (v, e) c
      pure ()



fmRandomPermutation :: IO ()
fmRandomPermutation = do
  n <- generate $ choose (1000, 10000)
  v <- pure $ V.generate n id
  u <- nonDeterministic $ randomPermutation n
  a <- V.thaw u
  V.sort a
  w <- V.freeze a

  assertBool "permutation" $ u /= v
  assertBool "sorted"      $ w == v



fmMatch :: IO ()
fmMatch = do
  (v, e) <- arbitraryHypergraph 10000
  clustering <- nonDeterministic $ do
      u <- randomPermutation $ length v
      st $ match (v, e) matchingRatio u

  assertEqual "length does not match" (length v) (sum $ size <$> clustering)
  assertBool "elements do not match" $ foldMap id clustering == fromAscList [0 .. length v - 1]
  assertBool "clustering" $ length clustering <= length v



fmInputRoutine :: IO ()
fmInputRoutine = void $ arbitraryHypergraph 10000


fmDeterministic :: IO ()
fmDeterministic = do

  h <- stToIO queue_1Hypergraph
  p <- stToIO $ evalFM $ fiducciaMattheyses h

  assertEqual "cut size" 11 $ cutSize h p



blifHypergraph :: BLIF -> ST s (V, E)
blifHypergraph netlist = inputRoutine
    (top ^. nets . to length)
    (top ^. gates . to length)
    [ (n, c)
    | (n, w) <- zip [0..] $ toList $ top ^. nets
    , (c, _) <- w ^. contacts . to assocs
    ] where top = fromBLIF netlist



arbitraryHypergraph :: Int -> IO (V, E)
arbitraryHypergraph n = do
  k <- generate $ choose (1, n)
  y <- generate $ choose (k, k+n)
  config <- generate $ vectorOf (4 * k) $ (,) <$> choose (0, pred y) <*> choose (0, pred k)
  stToIO $ inputRoutine y k config


queue_1Hypergraph :: ST s (V, E)
queue_1Hypergraph = either (fail . show) blifHypergraph $ parseBLIF queue_1Blif

picorv32Hypergraph :: ST s (V, E)
picorv32Hypergraph = either (fail . show) blifHypergraph $ parseBLIF picorv32Blif


picorv32Blif :: Text
picorv32Blif = decodeUtf8 $(embedFile "sample/picorv32.blif")

queue_1Blif :: Text
queue_1Blif = decodeUtf8 $(embedFile "sample/queue_1.blif")





concurrency :: TestTree
concurrency = testGroup "Concurrency" $
  [ testCase "Dual core" $ dualCore n
  | n <- take 4 $ drop 2 $ iterate (`shiftR` 1) (shiftL 1 20)
  ] ++
  [ testCase "Quad core" $ quadCore n
  | n <- take 4 $ drop 2 $ iterate (`shiftR` 1) (shiftL 1 20)
  ] ++
  [ testCase "Stream processor" $ stream n
  | n <- take 4 $ drop 2 $ iterate (`shiftR` 1) (shiftL 1 20)
  ] ++
  [ testCase "Remote procedure" $ remoteProcess n
  | n <- take 4 $ drop 2 $ iterate (`shiftR` 1) (shiftL 1 20)
  ] ++
  [ testCase "Race condition" $ raceCondition n
  | n <- take 4 $ drop 2 $ iterate (`shiftR` 1) (shiftL 1 20)
  ]


dualCore :: Int -> IO ()
dualCore n = do
  counter <- newMVar 0
  ws <- createWorkers 2
  let inc = local_ $ incrementWithDelay (2*n) counter
      act = fst ^<< inc &&& inc &&& inc &&& inc &&& inc &&& inc
      tech = thaw def
      opts = def & workers .~ ws
  _ <- forkIO $ runLSC opts tech $ compiler act mempty
  threadDelay (3*n)
  result1 <- readMVar counter
  threadDelay (2*n)
  result2 <- readMVar counter
  result1 @?= 2
  result2 @?= 4


quadCore :: Int -> IO ()
quadCore n = do
  counter <- newMVar 0
  ws <- createWorkers 4
  let inc = local_ $ incrementWithDelay (2*n) counter
      act = fst . snd ^<< (inc &&& inc) &&& inc &&& (inc &&& inc &&& inc)
      tech = thaw def
      opts = def & workers .~ ws
  _ <- forkIO $ runLSC opts tech $ compiler act mempty
  threadDelay (3*n)
  result1 <- readMVar counter
  threadDelay (5*n)
  result2 <- readMVar counter
  result1 @?= 4
  result2 @?= 6


stream :: Int -> IO ()
stream n = do
  counter <- newMVar 0
  ws <- createWorkers 4
  let inc = local_ $ incrementWithDelay (2*n) counter
      act = select inc
      tech = thaw def
      opts = def & workers .~ ws
  _ <- forkIO $ runLSC opts tech $ () <$ compiler act (replicate 8 ())
  threadDelay (3*n)
  result1 <- readMVar counter
  threadDelay (2*n)
  result2 <- readMVar counter
  result1 @?= 4
  result2 @?= 8


remoteProcess :: Int -> IO ()
remoteProcess n = do
  counter1 <- newMVar 0
  counter2 <- newMVar 0
  ws <- createWorkers 2
  let inc1 = local_ $ incrementWithDelay (2*n) counter1
      inc2 = remote_ $ incrementWithDelay (2*n) counter2
      act = fst ^<< inc1 &&& inc1 &&& inc2 &&& inc1 &&& inc2 &&& inc1 &&& inc1 &&& inc1 &&& inc1 &&& inc1 &&& inc1
      tech = thaw def
      opts = def & workers .~ ws
  _ <- forkIO $ runLSC opts tech $ () <$ compiler act mempty
  threadDelay (7*n)
  result1 <- readMVar counter1
  result2 <- readMVar counter2
  result1 @?= 6
  result2 @?= 2


raceCondition :: Int -> IO ()
raceCondition n = do
  counter <- newMVar 0
  ws <- createWorkers 4
  let inc k = local_ $ incrementWithDelay (2*k*n) counter
      act = inc 1 \\\ inc 3 \\\ inc 3 \\\ inc 4 \\\ inc 5 \\\ inc 6
      tech = thaw def
      opts = def & workers .~ ws
  runLSC opts tech $ compiler act mempty
  result1 <- readMVar counter
  threadDelay (8*n)
  result2 <- readMVar counter
  result1 @?= 1
  result1 @?= result2


incrementWithDelay :: Int -> MVar Int -> LSC ()
incrementWithDelay n counter = liftIO $ do
  threadDelay n
  modifyMVar_ counter $ pure . succ


