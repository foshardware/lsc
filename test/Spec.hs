
import Control.Category
import Control.Arrow
import Control.Arrow.Select
import Control.Concurrent
import Control.Lens
import Control.Monad.IO.Class
import Data.Bits
import Data.Default
import Prelude hiding (id, (.))

import Test.Tasty
import Test.Tasty.HUnit

import LSC
import LSC.Types


main :: IO ()
main = defaultMain $ testGroup "LSC"
  [ concurrency
  ]


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
  ]


dualCore :: Int -> IO ()
dualCore n = do
  counter <- newMVar 0
  ws <- createWorkers 2
  let inc = incrementWithDelay (2*n) counter
      act = fst ^<< inc &&& inc &&& inc &&& inc
      tech = thaw def
      opts = def & workers .~ ws
  _ <- forkIO $ runLSC opts tech $ compiler act mempty
  threadDelay n
  result1 <- readMVar counter
  threadDelay (2*n)
  result2 <- readMVar counter
  assertBool "two at a time" $ result1 == 2 && result2 == 4


quadCore :: Int -> IO ()
quadCore n = do
  counter <- newMVar 0
  ws <- createWorkers 4
  let inc = incrementWithDelay (2*n) counter
      act = fst . snd ^<< (inc &&& inc) &&& inc &&& (inc &&& inc &&& inc)
      tech = thaw def
      opts = def & workers .~ ws
  _ <- forkIO $ runLSC opts tech $ compiler act mempty
  threadDelay n
  result1 <- readMVar counter
  threadDelay (2*n)
  result2 <- readMVar counter
  assertBool "four at a time" $ result1 == 4 && result2 == 6


stream :: Int -> IO ()
stream n = do
  counter <- newMVar 0
  ws <- createWorkers 4
  let inc = incrementWithDelay (2*n) counter
      act = select inc
      tech = thaw def
      opts = def & workers .~ ws
  _ <- forkIO $ runLSC opts tech $ () <$ compiler act (replicate 8 ())
  threadDelay n
  result1 <- readMVar counter
  threadDelay (2*n)
  result2 <- readMVar counter
  assertBool "streaming" $ result1 == 4 && result2 == 8


incrementWithDelay :: Int -> MVar Int -> Compiler ()
incrementWithDelay n counter = ls_ . liftIO $ do
  modifyMVar_ counter $ pure . succ
  threadDelay n


