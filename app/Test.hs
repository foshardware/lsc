{-# LANGUAGE TemplateHaskell #-}

module Test where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

import Data.FileEmbed
import Data.Text (Text)
import Data.Text.Encoding

import BLIF.Builder
import BLIF.Parser
import LEF.Parser

import LSC
import LSC.BLIF
import LSC.LEF
import LSC.Exlining
import LSC.Types


type Test = MaybeT IO

runTests :: IO ()
runTests = void $ runMaybeT tests

tests :: Test ()
tests = do

  lefOsu035 <- lift $ either
    (ioError . userError . show)
    (pure . fromLEF)
    (parseLEF osu035)

  blifRot <- lift $ either
    (ioError . userError . show)
    (pure . gnostic lefOsu035 . fromBLIF)
    (parseBLIF rot)

  liftIO $ printBLIF $ toBLIF $ exlineRounds (replicate 3 4) blifRot


rot :: Text
rot = decodeUtf8 $(embedFile "tests/rot.blif.test")

osu035 :: Text
osu035 = decodeUtf8 $(embedFile "tests/osu035.lef.test")

