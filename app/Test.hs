{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

import Data.FileEmbed
import Data.Text (Text)
import Data.Text.Encoding
import Data.Text.Lazy.Builder

import System.Exit
import System.IO

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
main = exitWith . maybe (ExitFailure 1) (const ExitSuccess) =<< runMaybeT tests

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

  let exlined = exlineRounds (repeat 20) blifRot
  let inlined = inlineAll exlined
  liftIO $ printBLIF $ toBLIF $ exlined
  it "inlines correctly" (reprBlif inlined == reprBlif blifRot)
    $ liftIO $ printBLIF $ toBLIF $ inlined

  where

    reprBlif = toLazyText . builderBlif . toBLIF


it :: String -> Bool -> Test () -> Test ()
it desc True _ = do
  liftIO $ hPutStrLn stderr $ unwords ["  ✔", "\tit", desc]
it desc b action = do
  liftIO $ hPutStrLn stderr $ unwords ["\n  ✖", "\tFAIL:", desc, "\n\n"]
  action
  guard b


it_ :: String -> Bool -> Test ()
it_ desc b = it desc b $ pure ()


rotFile :: Text
rotFile = decodeUtf8 $(embedFile "tests/rot.blif.test")

osu035File :: Text
osu035File = decodeUtf8 $(embedFile "tests/osu035.lef.test")

