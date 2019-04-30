
import Control.Category
import Control.Arrow
import Control.Arrow.Select
import Control.Concurrent
import Control.Lens
import Control.Monad.ST
import Control.Monad.IO.Class
import Data.Bits
import Data.Default
import Prelude hiding (id, (.))

import Test.Tasty
import Test.Tasty.HUnit

import LSC
import LSC.Types
import LSC.FM


main :: IO ()
main = defaultMain $ testGroup "LSC"
  [ fm
  , concurrency
  ]


fm :: TestTree
fm = testGroup "FM" $
  [ testCase "inputRoutine" $ fmInputRoutine
  ]

fmInputRoutine :: IO ()
fmInputRoutine = do
  result <- stToIO $ fmap (fmap $ view partitioning) $ execFM $ bipartition =<< inputRoutine 5 6
    [ (0,3), (0,4)
    , (1,1), (1,4)
    , (2,0), (2,1), (2,2)
    , (3,1), (3,5)
    , (4,1), (4,2), (4,3)
    ]
  putStrLn $ show result


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


