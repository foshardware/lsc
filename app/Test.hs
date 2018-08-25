
module Test where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

import LSC.Exlining


type Test = MaybeT IO

runTests :: IO ()
runTests = void $ runMaybeT tests

tests :: Test ()
tests = pure ()

