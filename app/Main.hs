{-# LANGUAGE TupleSections #-}

module Main where

import Control.Lens

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Concurrent
import Data.Default
import Data.Either
import qualified Data.ByteString.Lazy.Char8 as Bytes
import qualified Data.Text.IO as Text
import System.Console.GetOpt
import System.Environment
import System.IO
import Text.Parsec (parse)
import Text.ParserCombinators.Parsec.Number (decimal)

import LSC.BLIF    (parseBLIF, toBLIF)
import LSC.LEF     (parseLEF, fromLEF)
import LSC.Verilog (parseVerilog)

import LSC
import LSC.BLIF
import LSC.D3
import LSC.Easy
import LSC.Exline
import LSC.FIR
import LSC.SVG
import LSC.Types
import LSC.Version


type App = MaybeT IO
main :: IO ()
main = void $ runMaybeT program

exit :: App ()
exit = guard False

program :: App ()
program = do

  (flags, _) <- liftIO $ compilerFlags =<< getArgs
  opts <- liftIO $ compilerOpts flags

  let arg x = or [ k == x | (k, _) <- flags ]
      str x = head [ v | (k, v) <- flags, k == x ]

  when (null flags) exit

  -- print version string
  when (arg Version)
    $ do
      liftIO $ hPutStrLn stderr $ versionString
      exit

  -- firrtl synthesis
  when (arg Exline && arg Firrtl)
    $ do
      fir_ <- liftIO $ Text.readFile $ str Firrtl
      liftIO $ either
        (ioError . userError . show)
        (putStrLn . show)
        (parseFIR fir_)
      exit

  -- generate registers
  when (arg Register && arg Lef)
    $ do
      lef_ <- liftIO $ Text.readFile $ str Lef
      liftIO $ hPutStrLn stderr $ show $ parseLEF lef_
      exit

  -- json report
  when (arg Json && arg Verilog)
    $ do
      verilog_ <- liftIO $ Text.readFile $ str Verilog
      liftIO $ Bytes.putStrLn $ encodeVerilog $ parseVerilog verilog_
      exit

   -- svg output
  when (arg Lef && arg Blif)
    $ do
      net_ <- liftIO $ Text.readFile $ str Blif
      lef_ <- liftIO $ Text.readFile $ str Lef

      tech <- liftIO $ either
        (ioError . userError . show)
        (pure . fromLEF)
        (parseLEF lef_)
      netlist <- liftIO $ either
        (ioError . userError . show)
        (pure . fromBLIF)
        (parseBLIF net_)

      when (arg Exline)
        $ do
          new <- liftIO $ evalLSC opts tech $ exline netlist
          liftIO $ printBLIF $ toBLIF new
          exit

      when (arg LayoutEstimation)
        $ do
          circuit2d <- liftIO $ evalLSC opts tech $ compiler layoutEstimation netlist
          liftIO $ plotStdout circuit2d
          exit

      when (arg Compile)
        $ do
          circuit2d <- lift $ evalLSC opts tech $ compiler stage1 netlist
          liftIO $ plotStdout circuit2d
          exit


  when (arg Exline && not (arg Blif) && not (arg Firrtl))
    $ do
      liftIO $ hPutStrLn stderr "exline: no rtl given"
      exit

  when (arg Exline && not (arg Lef))
    $ do
      liftIO $ hPutStrLn stderr "exline: no tech given"
      exit

  when (arg Json && arg Blif)
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
  | LayoutEstimation
  | Compile
  | Visuals
  | Iterations
  | CutRatio
  | Smt
  | Cores
  | Debug
  | Register
  | Rtl
  | Firrtl
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
    , Option [ ]        ["estimate-layout"] (NoArg (LayoutEstimation, mempty)) "estimate area"

    , Option ['d']      ["debug"]      (NoArg  (Debug, mempty))     "print some debug info"
    , Option ['g']      ["visuals"]    (NoArg  (Visuals, mempty))   "show visuals"
    , Option ['x']      ["exline"]
        (OptArg ((Exline, ) . maybe "top" id)   "component")        "just exline and exit"
    , Option [ ]        ["cut-ratio"]
        (OptArg  ((CutRatio, ) . maybe "40" id) "n")                "max bound cut sizes"

    , Option ['i']      ["iterations"]
        (OptArg  ((Iterations, ) . maybe "4" id) "n")               "iterations"

    , Option ['c']      ["compile"]
        (OptArg ((Compile,  ) . maybe "svg" id) "svg,magic")        "output format"

    , Option ['s']      ["smt"]        (ReqArg (Smt, ) "yices,z3")  "specify smt backend"
    , Option ['j']      ["cores"]      (ReqArg (Cores,  ) "count")  "limit number of cores"
    , Option ['J']      ["json"]       (NoArg  (Json, mempty))      "export json"
    , Option ['f']      ["firrtl"]     (ReqArg (Firrtl, ) "FILE")   "firrtl file"
    , Option ['u']      ["verilog"]    (ReqArg (Verilog, ) "FILE")  "verilog file"
    ]


compilerOpts :: [Flag] -> IO CompilerOpts
compilerOpts xs = do

  n <- getNumCapabilities
  let j = last $ n : rights [ parse decimal "-j" v | (k, v) <- xs, k == Cores ]
  setNumCapabilities j
  ws <- createWorkers j

  let i = last $ 4 : rights [ parse decimal "-i" v | (k, v) <- xs, k == Iterations ]

  let c = last $ 40 : rights [ parse decimal "--cut-ratio" v | (k, v) <- xs, k == CutRatio ]

  pure $ def
    & enableDebug .~ elem Debug (fst <$> xs)
    & enableVisuals .~ elem Visuals (fst <$> xs)
    & iterations .~ i
    & cutRatio .~ c
    & workers .~ ws


compilerFlags :: [String] -> IO ([Flag], [String])
compilerFlags argv =
    case getOpt Permute args argv of
        (o, n, []  ) -> pure (o, n)
        (_, _, errs) -> mempty <$ hPutStrLn stderr (concat errs ++ usageInfo header args)
     where header = "Usage: lsc [arg...]"
