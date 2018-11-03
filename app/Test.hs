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
import LSC.NetGraph
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

  blifPicorv32 <- lift $ either
    (ioError . userError . show)
    (pure . gnostic lefOsu035 . fromBLIF)
    (parseBLIF picorv32File)

  let exlined = exline_ (replicate 64 8) blifPicorv32
  let inlined = inlineAll exlined
  -- liftIO $ printBLIF $ toBLIF $ exlined
  liftIO $ hPutStrLn stderr $ showNetHierarchy $ exlined
  it "inlines correctly" (reprBlif inlined == reprBlif blifPicorv32)
    $ liftIO $ printBLIF $ toBLIF $ inlined

  where

    reprBlif = toLazyText . builderBlif . toBLIF


it :: String -> Bool -> Test () -> Test ()
it desc True _ = do
  liftIO $ hPutStrLn stderr $ unwords ["  v", "\tit", desc]
it desc b action = do
  liftIO $ hPutStrLn stderr $ unwords ["\n  x", "\tFAIL:", desc, "\n\n"]
  action
  guard b


it_ :: String -> Bool -> Test ()
it_ desc b = it desc b $ pure ()


picorv32File :: Text
picorv32File = decodeUtf8 $(embedFile "tests/picorv32.blif")

osu035File :: Text
osu035File = decodeUtf8 $(embedFile "tests/osu035.lef")

