{-# LANGUAGE TemplateHaskell #-}

module Test where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

import Data.FileEmbed
import Data.Text (Text)
import Data.Text.Encoding
import qualified Data.Text.IO as Text

import LSC.Exlining


type Test = MaybeT IO

runTests :: IO ()
runTests = void $ runMaybeT tests

tests :: Test ()
tests = do

  liftIO $ Text.putStrLn rot


rot :: Text
rot = decodeUtf8 $(embedFile "tests/rot.blif.test")

