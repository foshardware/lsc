{-# LANGUAGE GADTs, DataKinds, TupleSections, DeriveDataTypeable #-}

module Main where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

import Data.Foldable (for_)

import qualified Data.ByteString.Lazy as Bytes

import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy.IO as Pipe

import System.Console.GetOpt
import System.Environment
import System.IO
import System.Process

import BLIF.Builder
import BLIF.Parser
import LEF.Parser
import Verilog.Parser

import LSC
import LSC.BLIF
import LSC.D3
import LSC.LEF
import LSC.SVG
import LSC.Exlining
import LSC.NetGraph
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

  let arg x = elem x $ fst <$> opts

  -- print version string
  when (arg Version)
    $ do
      liftIO $ hPutStrLn stderr $ versionString
      exit

  -- run tests
  when (arg Test)
    $ do
      liftIO $ withCreateProcess (proc "lsc-test" []) { std_out = CreatePipe }
        $ \ _ hout _ _ -> for_ hout
        $ \ out -> Pipe.hGetContents out >>= Pipe.hPutStr stdout
      exit

  -- json report
  when (and $ arg <$> [Json, Verilog])
    $ do
      verilog_ <- liftIO $ Text.readFile $ head [v | (k, v) <- opts, k == Verilog ]
      liftIO $ Bytes.putStrLn $ encodeVerilog $ parseVerilog verilog_
      exit

  when (and $ arg <$> [Json, Lef, Blif, Exline])
    $ do
      net_ <- liftIO $ Text.readFile $ head [v | (k, v) <- opts, k == Blif]
      lef_ <- liftIO $ Text.readFile $ head [v | (k, v) <- opts, k == Lef ]
      tech <- lift $ either
        (ioError . userError . show)
        (pure . fromLEF)
        (parseLEF lef_)
      netlist <- liftIO $ either
        (ioError . userError . show)
        (Bytes.putStrLn . encodeNetGraph . gnostic tech . fromBLIF)
        (parseBLIF net_)
      exit

  when (arg Exline && not (arg Blif))
    $ do
      liftIO $ hPutStrLn stderr "exline: no blif given"
      exit

  when (arg Exline && not (arg Lef))
    $ do
      liftIO $ hPutStrLn stderr "exline: no lef given"
      exit

  when (and $ arg <$> [Json, Blif])
    $ do
      blif_ <- liftIO $ Text.readFile $ head [v | (k, v) <- opts, k == Blif]
      liftIO $ either
        (ioError . userError . show)
        (Bytes.putStrLn . encodeBLIF)
        (parseBLIF blif_)
      exit



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
  | Verilog
  | Json
  deriving (Eq, Show)

type FlagValue = String

args :: [OptDescr Flag]
args =
    [ Option ['v']      ["verbose"]    (NoArg  (Verbose, mempty))   "chatty output on stderr"
    , Option ['V', '?'] ["version"]    (NoArg  (Version, mempty))   "show version number"
    , Option ['b']      ["blif"]       (ReqArg (Blif, ) "FILE")     "BLIF file"
    , Option ['l']      ["lef"]        (ReqArg (Lef,  ) "FILE")     "LEF file"
    , Option ['d']      ["debug"]      (NoArg  (Debug, []))         "print some debug info"
    , Option ['c']      ["compile"]
        (OptArg ((Compile,  ) . maybe "svg" id) "svg,magic")        "output format"

    , Option ['x']      ["exline"]
        (OptArg ((Exline, ) . maybe "top" id)   "component")        "just exline and exit"

    , Option ['t']      ["test"]       (NoArg  (Test, mempty))      "run tests and exit"
    , Option ['j']      ["cores"]
        (OptArg ((Cores,  ) . maybe "1" id) "count")                "use concurrency"

    , Option ['J']      ["json"]       (NoArg  (Json, mempty))      "export json"
    , Option ['u']      ["verilog"]    (ReqArg (Verilog, ) "FILE")  "verilog file"
    ]

exlineArgs :: String -> [String]
exlineArgs [] = []
exlineArgs string = arg : exlineArgs xs
  where (arg, _ : xs) = break (== ',') string

compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts argv =
    case getOpt Permute args argv of
        (o, n, []  ) -> pure (o, n)
        (_, _, errs) -> mempty <$ hPutStrLn stderr (concat errs ++ usageInfo header args)
     where header = "Usage: lsc [arg...]"

