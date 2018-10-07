{-# LANGUAGE GADTs, DataKinds, TupleSections, DeriveDataTypeable #-}

module Main where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

import Data.Foldable (for_)

import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy.IO as Pipe

import System.Console.GetOpt
import System.Environment
import System.IO
import System.Process

import BLIF.Builder
import BLIF.Parser
import LEF.Parser

import LSC
import LSC.BLIF
import LSC.LEF
import LSC.SVG
import LSC.Exlining
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

    when (null opts) exit

    -- print version string
    when (Version `elem` fmap fst opts)
      $ do
        liftIO $ hPutStrLn stderr $ versionString
        exit

    -- run tests
    when (Test `elem` fmap fst opts)
      $ do
        liftIO $ withCreateProcess (proc "lsc-test" []) { std_out = CreatePipe }
         $ \ _ hout _ _ -> for_ hout
          $ \ out -> Pipe.hGetContents out >>= Pipe.hPutStr stdout
        exit

    let lefFiles   = [v | (k, v) <- opts, k == Lef ]
        blifFiles  = [v | (k, v) <- opts, k == Blif]

    lef_ <- liftIO $ Text.readFile $ head lefFiles
    net_ <- liftIO $ Text.readFile $ head blifFiles

    tech <- lift $ either
        (ioError . userError . show)
        (pure . fromLEF)
        (parseLEF lef_)

    netlist <- lift $ either
        (ioError . userError . show)
        (pure . gnostic tech . fromBLIF)
        (parseBLIF net_)

    -- print exlined blif to stdout
    when (Exline `elem` fmap fst opts)
      $ do
        liftIO $ printBLIF $ toBLIF $ exline (replicate 3 4) netlist
        exit

    -- print debug info
    when (Debug `elem` fmap fst opts)
      $ lift $ runLSC tech $ debug ["start debug output on stderr"]

    -- use concurrency
    let j = maybe 1 read $ lookup Cores opts

    -- svg output
    circuit2d <- lift $ runLSC
      ( do
        tech
        bootstrap $ \ t -> t { enableDebug = Debug `elem` fmap fst opts } )
      ( stage1 j netlist )

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
  | Exline
  | Compile
  | Debug
  | Cores
  | Test
  | Rtl
  deriving (Eq, Show)

type FlagValue = String

options :: [OptDescr Flag]
options =
    [ Option ['v']      ["verbose"]    (NoArg  (Verbose, []))      "chatty output on stderr"
    , Option ['V', '?'] ["version"]    (NoArg  (Version, []))      "show version number"
    , Option ['b']      ["blif"]       (ReqArg (Blif, ) "FILE")    "BLIF file"
    , Option ['l']      ["lef"]        (ReqArg (Lef,  ) "FILE")    "LEF file"
    , Option ['d']      ["debug"]      (NoArg  (Debug, []))        "print some debug info"
    , Option ['c']      ["compile"]
        (OptArg ((Compile,  ) . maybe "svg" id) "svg,magic")       "output format"
    , Option ['x']      ["exline"]     (NoArg  (Exline, []))       "just exline and exit"
    , Option ['t']      ["test"]       (NoArg  (Test, []))         "run tests and exit"
    , Option ['j']      ["cores"]
        (OptArg ((Cores,  ) . maybe "1" id) "count")               "use concurrency"
    ]

compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts argv =
    case getOpt Permute options argv of
        (o, n, []  ) -> pure (o, n)
        (_, _, errs) -> mempty <$ hPutStrLn stderr (concat errs ++ usageInfo header options)
     where header = "Usage: lsc [OPTION...] files..."

