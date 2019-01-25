{-# LANGUAGE TupleSections #-}

module Main where

import Control.Lens

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

import Data.Either (rights)
import Data.Foldable (for_)
import Data.Default

import qualified Data.ByteString.Lazy.Char8 as Bytes

import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy.IO as Pipe

import System.Console.GetOpt
import System.Environment
import System.IO
import System.Process

import Text.Parsec (parse)

import BLIF.Parser
import LEF.Parser
import Verilog.Parser

import LSC
import LSC.BLIF
import LSC.D3
import LSC.LEF
import LSC.SVG
import LSC.Numbers
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

  (flags, _) <- liftIO $ compilerFlags =<< getArgs
  env <- liftIO $ compilerOpts flags

  let arg x = or [ k == x | (k, _) <- flags ]
      str x = head [ v | (k, v) <- flags, k == x ]

  when (null flags) exit

  -- print version string
  when (arg Version)
    $ do
      liftIO $ hPutStrLn stderr $ versionString
      exit

  -- generate registers
  when (and $ arg <$> [Register, Lef])
    $ do
      lef_ <- liftIO $ Text.readFile $ str Lef
      liftIO $ hPutStrLn stderr $ show $ parseLEF lef_
      exit

  -- json report
  when (and $ arg <$> [Json, Verilog])
    $ do
      verilog_ <- liftIO $ Text.readFile $ str Verilog
      liftIO $ Bytes.putStrLn $ encodeVerilog $ parseVerilog verilog_
      exit

   -- svg output
  when (and $ arg <$> [Lef, Blif, Compile])
    $ do
      net_ <- liftIO $ Text.readFile $ str Blif
      lef_ <- liftIO $ Text.readFile $ str Lef

      tech <- lift $ either
        (ioError . userError . show)
        (pure . fromLEF)
        (parseLEF lef_)
      netlist <- liftIO $ either
        (ioError . userError . show)
        (pure . gnostic tech . fromBLIF)
        (parseBLIF net_)

      circuit2d <- lift $ evalLSC env tech $ compiler stage1 netlist

      liftIO $ plotStdout circuit2d

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
      blif_ <- liftIO $ Text.readFile $ str Blif
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
  | Register
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
    , Option ['d']      ["debug"]      (NoArg  (Debug, mempty))     "print some debug info"
    , Option ['c']      ["compile"]
        (OptArg ((Compile,  ) . maybe "svg" id) "svg,magic")        "output format"

    , Option ['x']      ["exline"]
        (OptArg ((Exline, ) . maybe "top" id)   "component")        "just exline and exit"

    , Option ['J']      ["json"]       (NoArg  (Json, mempty))      "export json"
    , Option ['u']      ["verilog"]    (ReqArg (Verilog, ) "FILE")  "verilog file"
    , Option ['r']      ["register"]   (ReqArg (Register, ) "size in bits")  "generate register"
    ]


compilerOpts :: [Flag] -> IO CompilerOpts
compilerOpts xs = do
  ws <- rtsWorkers
  pure $ def
    & enableDebug .~ elem Debug (fst <$> xs)
    & workers .~ ws


compilerFlags :: [String] -> IO ([Flag], [String])
compilerFlags argv =
    case getOpt Permute args argv of
        (o, n, []  ) -> pure (o, n)
        (_, _, errs) -> mempty <$ hPutStrLn stderr (concat errs ++ usageInfo header args)
     where header = "Usage: lsc [arg...]"
