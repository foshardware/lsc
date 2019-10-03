{-# LANGUAGE TupleSections #-}

module Main where

import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Concurrent
import Data.Char
import Data.Default
import Data.Either
import qualified Data.Text.IO as Text
import System.Console.GetOpt
import System.Environment
import System.FilePath
import System.IO
import Text.Parsec (parse)
import Text.ParserCombinators.Parsec.Number (decimal)

import LSC.BLIF    (parseBLIF)
import LSC.LEF     (parseLEF, fromLEF)
import LSC.DEF     (printDEF, toDEF, fromDEF, parseDEF)

import LSC
import LSC.BLIF
import LSC.NetGraph
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

  (flags, inputs) <- liftIO $ compilerFlags =<< getArgs
  opts <- liftIO $ compilerOpts flags

  let arg x = or [ k == x | (k, _) <- flags ]
      list x = [ map toLower v | (k, v) <- flags, k == x ]

  when (null flags) exit

  -- print version string
  when (arg Version)
    $ do
      liftIO $ hPutStrLn stderr $ versionString
      exit


  when (arg Lef && not (null inputs))
    $ do

      tech <- liftIO $ either (ioError . userError . show) (pure . fromLEF) . parseLEF
          =<< Text.readFile (head $ list Lef)

      netlist <- liftIO $ case splitExtension <$> inputs of
          (path, extension) : _ -> do
            file <- Text.readFile $ path ++ extension
            case extension of
              ".blif" -> either (ioError . userError . show) pure $ fromBLIF <$> parseBLIF file
              ".def"  -> either (ioError . userError . show) pure $ fromDEF  <$> parseDEF file
              ""      -> ioError $ userError $ "no file extension: "++ path
              _       -> ioError $ userError $ "unknown file extension: "++ extension
          _ -> ioError $ userError "no input given"

      when (arg LayoutEstimation)
        $ do
          circuit2d <- liftIO $ evalLSC opts tech $ compiler stage1 netlist
          liftIO $ printStdout circuit2d $ list Output
          exit

      when (arg Compile)
        $ do
          circuit2d <- lift $ evalLSC opts tech $ compiler stage4 netlist
          liftIO $ printStdout circuit2d $ list Output
          exit

      circuit2d <- liftIO $ evalLSC opts tech $ gateGeometry netlist
      liftIO $ printStdout circuit2d $ list Output
      exit


  when (null inputs)
    $ do
      void $ liftIO $ ioError $ userError "no inputs given"
      exit

  unless (arg Lef)
    $ do
      void $ liftIO $ ioError $ userError "no library given"
      exit



printStdout :: NetGraph -> [FlagValue] -> IO ()
printStdout ckt ("def" : _) = printDEF $ toDEF ckt
printStdout ckt _ = plotStdout ckt



type Flag = (FlagKey, FlagValue)

data FlagKey
  = Verbose
  | Version
  | Blif
  | Lef
  | Def
  | LayoutEstimation
  | Compile
  | Output
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

    , Option ['l']      ["lef"]        (ReqArg (Lef,  ) "FILE")     "LEF file"
    , Option ['x']      ["estimate-layout"] (NoArg (LayoutEstimation, mempty)) "estimate area"

    , Option ['d']      ["debug"]      (NoArg  (Debug, mempty))     "print some debug info"
    , Option ['g']      ["visuals"]    (NoArg  (Visuals, mempty))   "show visuals"
    , Option ['i']      ["iterations"]
        (OptArg  ((Iterations, ) . maybe "4" id) "n")               "iterations"

    , Option ['c']      ["compile"]    (NoArg (Compile, mempty))    "compile"

    , Option ['o']      ["output"]
        (OptArg ((Output,  ) . maybe "svg" id) "svg,def,magic")     "output format"

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

  pure $ def &~ do
      enableDebug .= elem Debug (fst <$> xs)
      enableVisuals .= elem Visuals (fst <$> xs)
      iterations .= i
      workers .= ws


compilerFlags :: [String] -> IO ([Flag], [String])
compilerFlags argv =
    case getOpt Permute args argv of
        (o, n, []  ) -> pure (o, n)
        (_, _, errs) -> mempty <$ hPutStrLn stderr (concat errs ++ usageInfo header args)
     where header = "Usage: lsc [arg...] input"
