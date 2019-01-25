
import Control.Category
import Control.Arrow
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Lens
import Control.Monad.IO.Class
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
concurrency = testGroup "Concurrency"
  [ testCase "dual core" dualCore
  , testCase "quad core" quadCore
  ]


time :: Int -> Int
time n = n * 100000


dualCore :: IO ()
dualCore = do
  counter <- newMVar 0
  ws <- createWorkers 2
  let inc = incrementWithDelay 2 counter
      act = fst ^<< inc &&& inc &&& inc &&& inc
      tech = thaw def
      opts = def & workers .~ ws
  forkIO $ runLSC opts tech $ compiler act mempty
  threadDelay $ time 1
  result1 <- readMVar counter
  threadDelay $ time 2
  result2 <- readMVar counter
  assertBool "two at a time" $ result1 == 2 && result2 == 4


quadCore :: IO ()
quadCore = do
  counter <- newMVar 0
  ws <- createWorkers 4
  let inc = incrementWithDelay 2 counter
      act = fst . snd ^<< (inc &&& inc) &&& inc &&& (inc &&& inc &&& inc)
      tech = thaw def
      opts = def & workers .~ ws
  forkIO $ runLSC opts tech $ compiler act mempty
  threadDelay $ time 1
  result1 <- readMVar counter
  threadDelay $ time 2
  result2 <- readMVar counter
  assertBool "four at a time" $ result1 == 4 && result2 == 6


incrementWithDelay :: Int -> MVar Int -> Compiler ()
incrementWithDelay n counter = ls_ . liftIO $ do
  modifyMVar_ counter $ pure . succ
  threadDelay $ time n


