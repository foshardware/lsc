{-# LANGUAGE GADTs, DataKinds, TupleSections, DeriveDataTypeable #-}

module Main where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

import Data.Foldable

import qualified Data.Text.IO as Text

import System.Console.GetOpt
import System.Environment
import System.IO

import BLIF.Parser
import LEF.Parser

import LSC
import LSC.BLIF
import LSC.LEF
import LSC.SVG
import LSC.Types

versionString :: String
versionString = "lsc 0.1.0.0"

type App = MaybeT IO
main :: IO ()
main = void $ runMaybeT program

exit :: App ()
exit = guard False

program :: App ()
program = do

    (opts, _) <- liftIO $ compilerOpts =<< getArgs

    -- print version string
    when (Version `elem` fmap fst opts)
      $ do
        liftIO $ hPutStrLn stderr $ versionString
        exit

    let lefFiles   = [v | (k, v) <- opts, k == Lef ]
        blifFiles  = [v | (k, v) <- opts, k == Blif]

    lef_ <- liftIO $ Text.readFile $ head lefFiles
    net_ <- liftIO $ Text.readFile $ head blifFiles

    bootstr <- lift $ either
        (ioError . userError . show)
        (pure . fromLEF)
        (parseLEF lef_)

    netlist <- lift $ either
        (ioError . userError . show)
        (pure . gnostic bootstr . fromBLIF)
        (parseBLIF net_)

    -- print debug info
    when (Debug `elem` fmap fst opts)
      $ do
        liftIO $ hPutStrLn stderr $ show netlist
        exit

    -- svg output
    circuit2d <- lift $ bootstr `runLSC` stage1 netlist

    when (Compile `elem` fmap fst opts)
      $ do
        liftIO $ plotStdout circuit2d

    -- sat output
    when (Verbose `elem` fmap fst opts)
      $ do
        liftIO $ hPutStrLn stderr $ show circuit2d


type Flag = (FlagKey, FlagValue)

data FlagKey
  = Verbose
  | Version
  | Blif
  | Lef
  | Compile
  | Debug
  deriving (Eq, Show)

type FlagValue = String

options :: [OptDescr Flag]
options =
    [ Option ['v']      ["verbose"] (NoArg  (Verbose, []))  "chatty output on stderr"
    , Option ['V', '?'] ["version"] (NoArg  (Version, []))  "show version number"
    , Option ['b']      ["blif"]    (ReqArg (Blif, ) "FILE") "BLIF file"
    , Option ['l']      ["lef"]     (ReqArg (Lef,  ) "FILE") "LEF file"
    , Option ['c']      ["compile"] (OptArg ((Compile,  ) . maybe "svg" id) "svg,magic") "output format"
    , Option ['d']      ["debug"]   (NoArg  (Debug, []))    "output format"
    ]

compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts argv =
    case getOpt Permute options argv of
        (o, n, []  ) -> pure (o, n)
        (_, _, errs) -> ioError $ userError $ concat errs ++ usageInfo header options
     where header = "Usage: lsc [OPTION...] files..."

