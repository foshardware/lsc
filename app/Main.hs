{-# LANGUAGE GADTs, DataKinds, TupleSections, DeriveDataTypeable #-}

module Main where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

import Data.Text.IO as Text

import System.Console.GetOpt
import System.Environment

import BLIF.Parser
import LEF.Parser

import LSC
import LSC.BLIF
import LSC.LEF
import LSC.SVG
import LSC.Types

type App = MaybeT IO
main = runMaybeT program

program :: App ()
program = do

    (opts, _) <- liftIO $ compilerOpts =<< getArgs

    let lefFile   = head [v | (k, v) <- opts, k == Lef]
        blifFile  = head [v | (k, v) <- opts, k == Blif]

    result <- lift $ do
        bootstrap <- either (error . show) fromLEF . parseLEF <$> Text.readFile lefFile
        netlist   <- either (error . show) (gnostic bootstrap . fromBLIF) . parseBLIF <$> Text.readFile blifFile

        bootstrap `runLSC` stage1 netlist

    liftIO $ plotStdout $ lexNodes result

type Flag = (FlagKey, FlagValue)

data FlagKey
  = Verbose
  | Version
  | Blif
  | Lef
  deriving (Eq, Show)

type FlagValue = String

options :: [OptDescr Flag]
options =
    [ Option ['v']      ["verbose"] (NoArg  (Verbose, []))  "chatty output on stderr"
    , Option ['V', '?'] ["version"] (NoArg  (Version, []))  "show version number"
    , Option ['b']      ["blif"]    (ReqArg (Blif, ) "FILE") "BLIF file"
    , Option ['l']      ["lef"]     (ReqArg (Lef,  ) "FILE") "LEF file"
    ]

compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts argv =
    case getOpt Permute options argv of
        (o, n, []  ) -> pure (o, n)
        (_, _, errs) -> ioError $ userError $ concat errs ++ usageInfo header options
     where header = "Usage: lsc [OPTION...] files..."
