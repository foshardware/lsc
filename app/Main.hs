-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE TupleSections #-}

module Main where

import Control.Concurrent
import Control.Lens
import Control.Monad
import Data.Aeson (encode, eitherDecode)
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as Lazy
import Data.Default
import Data.List (intersperse)
import Data.Maybe
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
import LSC.Logger
import LSC.NetGraph
import LSC.SVG
import LSC.Types
import LSC.Version



main :: IO ()
main = program


program :: IO ()
program = do

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

      tech <- either (die . show) (pure . freeze . fromLEF) . parseLEF =<< Text.readFile (head $ lst Lef)

      let scale = tech ^. scaleFactor

      netlist <- readNetGraph inputs

      when (arg DetailedPlacement)
        $ do
          circuit2d <- evalLSC opts tech $ compiler stage2 netlist
          last $ printStdout scale circuit2d <$> lst Output
          exitSuccess

      when (arg GlobalRouting)
        $ do
          circuit2d <- evalLSC opts tech $ compiler stage3 =<< compiler stage2 netlist
          last $ printStdout scale circuit2d <$> lst Output
          exitSuccess

      when (arg Legalize)
        $ do
          circuit2d <- evalLSC opts tech $ compiler stage0 netlist
          last $ printStdout scale circuit2d <$> lst Output
          exitSuccess

      when (arg LayoutEstimation)
        $ do
          evalLSC opts tech $ estimations . rebuildEdges =<< gateGeometry netlist
          exitSuccess

      circuit2d <- evalLSC opts tech $ gateGeometry netlist
      last $ printStdout scale circuit2d <$> lst Output
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
            ""      -> die $ unwords ["missing file extension:", path]
            _       -> die $ unwords ["unknown file extension:", extension]



printStdout :: Double -> NetGraph -> String -> IO ()
printStdout scale ckt "def" = printDEF $ toDEF scale ckt
printStdout scale ckt "svg" = plotStdout scale ckt
printStdout _ ckt _ = Lazy.putStr $ encode ckt



type Flag = (FlagKey, FlagValue)

data FlagKey
  = Help
  | Verbose
  | LogLevel
  | Lef
  | Seed
  | Legalize
  | RowCapacity
  | GlobalRouting
  | DetailedPlacement
  | LayoutEstimation
  | Output
  | Visuals
  | Iterations
  deriving Eq

type FlagValue = String


header :: String
header = unlines
  [ versionString
  , "Usage: lsc [arg...] input"
  ]


args :: [OptDescr Flag]
args =
  [ Option ['h']      ["help"]                  (NoArg (Help, ""))
    "print usage"

  , Option ['v', 'd'] ["verbose"]               (NoArg (Verbose, ""))
    "verbose output on stderr"
  , Option []         ["log-level"]             (ReqArg (LogLevel, ) "0,1,2,3,4")
    "set log level"

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

  , Option ['s']      ["seed"]                  (NoArg (Seed, ""))
    "seed from stdin"

  , Option ['o']      ["output"]    (ReqArg (Output, ) (join . intersperse "," . take 3 $ outputFormats))
    "output format"

  ]


outputFormats :: [String]
outputFormats =
  [ "svg"
  , "def"
  , "json"
  ]


compilerOpts :: [Flag] -> IO CompilerOpts
compilerOpts flags = do

    let lst x = [ v | (k, v) <- flags, k == x ]

    let known = last $ True : map (`elem` outputFormats) (lst Output)
    unless known $ die $ unwords ["unknown format:", head $ lst Output]

    c <- last $ pure 1 : (either (die . show) pure . (parse fractional "--row-capacity") <$> lst RowCapacity)
    i <- last $ pure 3 : (either (die . show) pure . (parse decimal "-i") <$> lst Iterations)

    let ks = either (die . show) (pure . toEnum) . (parse decimal "--log-level") <$> lst LogLevel
    k <- last $ pure Warning : (pure Debug <$ lst Verbose) ++ ks

    j <- rtsWorkers

    n <- getNumCapabilities
    unless (n < 2 || null (lst Seed))
      $ die "cannot seed from file descriptor in concurrent setting"

    pure $ def &~ do
      logLevel .= k
      rowCapacity .= c
      iterations .= i
      workers .= j
      seedHandle .= listToMaybe (stdin <$ lst Seed)



compilerFlags :: [String] -> IO ([Flag], [String])
compilerFlags argv = case getOpt Permute args argv of
    (o, n, []) -> pure (o, n)
    (_, _, es) -> die $ concat es ++ usageInfo header args

