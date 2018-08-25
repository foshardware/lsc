{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

import Data.FileEmbed
import Data.Text (Text)
import Data.Text.Encoding
import Data.Text.Lazy.Builder


import BLIF.Builder
import BLIF.Parser
import LEF.Parser

import LSC.BLIF
import LSC.LEF
import LSC.Inlining
import LSC.Exlining
import LSC.Types


type Test = MaybeT IO

main :: IO ()
main = void $ runMaybeT tests

tests :: Test ()
tests = do

  lefOsu035 <- lift $ either
    (ioError . userError . show)
    (pure . fromLEF)
    (parseLEF osu035File)

  blifRot <- lift $ either
    (ioError . userError . show)
    (pure . gnostic lefOsu035 . fromBLIF)
    (parseBLIF rotFile)

  let exlined = exlineRounds (replicate 3 4) blifRot
  let inlined = inlineAll exlined
  it "inlines correctly" $ reprBlif inlined == reprBlif exlined

  where

    reprBlif = toLazyText . builderBlif . toBLIF


it :: String -> Bool -> Test ()
it desc True = do
  liftIO $ putStrLn $ unwords ["it", desc, "\t", "✔"]
it desc b = do
  liftIO $ putStrLn $ unwords ["FAIL:", desc, "\t", "✖"]
  guard b


rotFile :: Text
rotFile = decodeUtf8 $(embedFile "tests/rot.blif.test")

osu035File :: Text
osu035File = decodeUtf8 $(embedFile "tests/osu035.lef.test")

