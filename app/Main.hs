-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE TupleSections #-}

module Main where

import Control.Lens
import Control.Monad
import Control.Concurrent
import Data.Aeson (encode, eitherDecode)
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as Lazy
import Data.Default
import Data.Text.Encoding (encodeUtf8Builder)
import qualified Data.Text.IO as Text
import System.Console.GetOpt
import System.Environment
import System.FilePath
import System.Exit
import System.IO
import Text.Parsec (parse)
import Text.ParserCombinators.Parsec.Number (decimal, fractional)

import LSC.BLIF    (fromBLIF, parseBLIF)
import LSC.LEF     (parseLEF, fromLEF)
import LSC.DEF     (printDEF, toDEF, fromDEF, parseDEF)

import LSC
import LSC.NetGraph
import LSC.SVG
import LSC.Types
import LSC.Version



main :: IO ()
main = program


program :: IO ()
program = do

  hPutStrLn stderr $ versionString

  (flags, inputs) <- compilerFlags =<< getArgs
  opts <- compilerOpts flags

  let arg x = or [ k == x | (k, _) <- flags ]
      lst x = [ v | (k, v) <- flags, k == x ]

  when (arg Help)
    $ do
      hPutStrLn stderr $ usageInfo header args
      exitSuccess

  when (null flags) exitSuccess

  unless (arg Lef) $ die "no library given"

  when (arg Lef)
    $ do

      tech <- either (fail . show) (pure . fromLEF) . parseLEF =<< Text.readFile (head $ lst Lef)

      let scale = freeze tech ^. scaleFactor

      netlist <- readNetGraph inputs

      when (arg DetailedPlacement || arg GlobalRouting)
        $ do
          circuit2d <- evalLSC opts tech $ compiler stage23 netlist
          printStdout scale circuit2d $ lst Output
          exitSuccess

      when (arg GlobalRouting)
        $ do
          circuit2d <- evalLSC opts tech $ compiler stage3 netlist
          printStdout scale circuit2d $ lst Output
          exitSuccess

      when (arg Legalize)
        $ do
          circuit2d <- evalLSC opts tech $ compiler stage0 netlist
          printStdout scale circuit2d $ lst Output
          exitSuccess

      when (arg LayoutEstimation)
        $ do
          circuit2d <- evalLSC opts tech $ compiler estimate . rebuildEdges =<< gateGeometry netlist
          printStdout scale circuit2d $ lst Output
          exitSuccess

      circuit2d <- evalLSC opts tech $ gateGeometry netlist
      printStdout scale circuit2d $ lst Output
      exitSuccess



readNetGraph :: [FilePath] -> IO NetGraph
readNetGraph inputs = case splitExtension <$> inputs of
    [] -> die "no input given"
    (path, extension) : _ -> do
        file <- Text.readFile $ path ++ extension
        case extension of
            ".blif" -> either (die . show) pure $ fromBLIF <$> parseBLIF file
            ".def"  -> either (die . show) pure $ fromDEF <$> parseDEF file
            ".json" -> either (die . show) pure $ eitherDecode $ toLazyByteString $ encodeUtf8Builder file
            ""      -> die $ "missing file extension: " ++ path
            _       -> die $ "unknown file extension: " ++ extension



printStdout :: Double -> NetGraph -> [FlagValue] -> IO ()
printStdout scale ckt ("def" : _) = printDEF $ toDEF scale ckt
printStdout scale ckt ("svg" : _) = plotStdout scale ckt
printStdout _ ckt _ = Lazy.putStr $ encode ckt



type Flag = (FlagKey, FlagValue)

data FlagKey
  = Help
  | Debug
  | Lef
  | Legalize
  | RowCapacity
  | GlobalRouting
  | DetailedPlacement
  | LayoutEstimation
  | Output
  | Visuals
  | Iterations
  | Cores
  deriving Eq

type FlagValue = String


header :: String
header = "Usage: lsc [arg...] input"

args :: [OptDescr Flag]
args =
  [ Option ['v', 'd'] ["verbose"]               (NoArg (Debug, ""))
    "verbose output on stderr"
  , Option ['h']      ["help"]                  (NoArg (Help, ""))
    "print usage"

  , Option ['l']      ["lef"]                   (ReqArg (Lef, ) "FILE")
    "LEF file"

  , Option ['y']      ["legalize"]              (NoArg (Legalize, ""))
    "legalize"
  , Option []         ["row-capacity"]          (ReqArg (RowCapacity, ) "ratio")
    "set row capacity (e.g. 0.7)"

  , Option ['p']      ["detailed-placement"]    (NoArg (DetailedPlacement, ""))
    "detailed placement"
  , Option ['g']      ["global-routing"]        (NoArg (GlobalRouting, ""))
    "global routing"
  , Option ['x']      ["estimate-layout"]       (NoArg (LayoutEstimation, ""))
    "estimate area"

--    , Option ['g']      ["visuals"]    (NoArg  (Visuals, mempty))   "show visuals"
  , Option ['i']      ["iterations"]            (ReqArg (Iterations, ) "n")
    "iterations"
  , Option ['j']      ["cores"]                 (ReqArg (Cores, ) "count")
    "limit number of cores"

  , Option ['o']      ["output"]                (ReqArg (Output, ) "svg,def,magic")
    "output format"

  ]



compilerOpts :: [Flag] -> IO CompilerOpts
compilerOpts flags = do

    let lst x = [ v | (k, v) <- flags, k == x ]

    n <- pred . max 1 . fromIntegral <$> getNumCapabilities

    c <- last $ pure 1 : (either (die . show) pure . (parse fractional "--row-capacity") <$> lst RowCapacity)
    i <- last $ pure 3 : (either (die . show) pure . (parse decimal "-i") <$> lst Iterations)
    j <- last $ pure n : (either (die . show) pure . (parse decimal "-j") <$> lst Cores)

    ws <- createWorkers j

    pure $ def &~ do
      enableVisuals .= not (null $ lst Visuals)
      enableDebug .= not (null $ lst Debug)
      rowCapacity .= c
      iterations .= i
      workers .= ws



compilerFlags :: [String] -> IO ([Flag], [String])
compilerFlags argv = case getOpt Permute args argv of
    (o, n, []) -> pure (o, n)
    (_, _, es) -> die $ concat es ++ usageInfo header args

