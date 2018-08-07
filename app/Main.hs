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

program :: App ()
program = do

    (opts, _) <- liftIO $ compilerOpts =<< getArgs

    -- print version string
    when (any ( \ (k, _) -> k == Version) opts)
      $ liftIO $ hPutStrLn stderr $ versionString

    -- escape hatches
    guard $ any ( \ (k, _) -> k == Compile) opts
    guard $ not $ any ( \ (k, _) -> k == Version) opts

    let lefFiles   = [v | (k, v) <- opts, k == Lef]
        blifFiles  = [v | (k, v) <- opts, k == Blif]

    result <- lift $ do
        bootstr <- either (error . show) fromLEF . parseLEF <$> Text.readFile (head lefFiles)
        netlist <- either (error . show) (gnostic bootstr . fromBLIF) . parseBLIF <$> Text.readFile (head blifFiles)

        bootstr `runLSC` stage1 netlist

    -- svg output
    when (any ( \ (k, v) -> k == Compile && v == "svg") opts)
      $ liftIO $ plotStdout $ lexNodes result

    -- sat output
    when (any ( \ (k, _) -> k == Verbose) opts)
      $ liftIO $ hPutStrLn stderr $ show result
    

type Flag = (FlagKey, FlagValue)

data FlagKey
  = Verbose
  | Version
  | Blif
  | Lef
  | Compile
  deriving (Eq, Show)

type FlagValue = String

options :: [OptDescr Flag]
options =
    [ Option ['v']      ["verbose"] (NoArg  (Verbose, []))  "chatty output on stderr"
    , Option ['V', '?'] ["version"] (NoArg  (Version, []))  "show version number"
    , Option ['b']      ["blif"]    (ReqArg (Blif, ) "FILE") "BLIF file"
    , Option ['l']      ["lef"]     (ReqArg (Lef,  ) "FILE") "LEF file"
    , Option ['c']      ["compile"] (OptArg ((Compile,  ) . maybe "svg" id) "svg,magic") "output format"
    ]

compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts argv =
    case getOpt Permute options argv of
        (o, n, []  ) -> pure (o, n)
        (_, _, errs) -> ioError $ userError $ concat errs ++ usageInfo header options
     where header = "Usage: lsc [OPTION...] files..."

