-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE TupleSections #-}

module Main where

import Control.Arrow
import Control.Lens
import Control.Monad
import Data.Aeson (encode, eitherDecode)
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as Lazy
import Data.Default
import Data.List (intersperse)
import Data.Maybe
import Data.Scientific
import Data.Text.Encoding (encodeUtf8Builder)
import qualified Data.Text.IO as Text
import System.Console.GetOpt
import System.Environment
import System.FilePath
import System.Exit
import System.IO
import System.IO.Silently
import Text.Parsec (parse)
import Text.ParserCombinators.Parsec.Number (decimal, fractional)

import LSC.BLIF    (fromBLIF, parseBLIF)
import LSC.LEF     (parseLEF, fromLEF)
import LSC.DEF     (printDEF, toDEF, fromDEF, parseDEF)

import LSC
import LSC.Log
import LSC.Model
import LSC.NetGraph
import LSC.SVG
import LSC.Technology
import LSC.Transformer
import LSC.Version



main :: IO ()
main = do

  hSetBuffering stdout $ BlockBuffering Nothing
  hSetBuffering stderr $ BlockBuffering Nothing

  withStderrLog program


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

      tech <- either (die . show) (deepFreeze . fromLEF) . parseLEF
          <=< Text.readFile . head $ lst Lef

      let scale = views scaleFactor toRealFloat tech

      netlist <- readNetGraph inputs

      when (arg Stage3 && arg Stage4)
        $ do
          ckt <- silence
            $ evalLSC opts tech
            $ compiler (stage3 >>> stage4) netlist
          last $ printStdout scale ckt <$> lst Output
          exitSuccess

      when (arg Stage3)
        $ do
          ckt <- silence
            $ evalLSC opts tech
            $ compiler stage3 netlist
          last $ printStdout scale ckt <$> lst Output
          exitSuccess

      when (arg Stage4)
        $ do
          ckt <- silence
            $ evalLSC opts tech
            $ compiler stage3 netlist
          last $ printStdout scale ckt <$> lst Output
          exitSuccess

      ckt <- silence
        $ evalLSC opts tech
        $ compiler (induce gateGeometry >>> arr rebuildHyperedges >>> estimate) netlist
      last $ printStdout scale ckt <$> lst Output
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



printStdout :: Double -> NetGraph -> String -> IO ()
printStdout scale ckt "def" = printDEF $ toDEF scale ckt
printStdout scale ckt "svg" = plotStdout scale ckt
printStdout _ ckt _ = Lazy.putStr $ encode ckt



type Flag = (FlagKey, FlagValue)

data FlagKey
  = Help
  | LogLevel
  | Stage3
  | Stage4
  | Lef
  | Seed
  | RowCapacity
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

  , Option ['q']      ["quiet"]                 (NoArg (LogLevel, "0"))
    "stay quiet"

  , Option ['v', 'd'] ["verbose"]               (NoArg (LogLevel, "4"))
    "verbose output on stderr"
  , Option []         ["log-level"]             (ReqArg (LogLevel, ) "0,1,2,3,4")
    "set log level"

  , Option ['l']      ["lef"]                   (ReqArg (Lef, ) "FILE")
    "LEF file"

  , Option []         ["row-capacity"]          (ReqArg (RowCapacity, ) "ratio")
    "set row capacity (e.g. 0.7)"

  , Option ['p']      ["stage3"]                (NoArg (Stage3, ""))
    "detailed placement"
  , Option ['g']      ["stage4"]                (NoArg (Stage4, ""))
    "routing"

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
    unless known $ die $ "unknown format: " ++ head (lst Output)

    c <- last $ pure 1 : (either (die . show) pure . parse fractional "--row-capacity" <$> lst RowCapacity)
    i <- last $ pure 3 : (either (die . show) pure . parse decimal "-i" <$> lst Iterations)

    let ks = either (die . show) (pure . toEnum) . parse decimal "--log-level" <$> lst LogLevel
    k <- last $ pure Warning : ks

    j <- rtsWorkers

    pure $ def &~ do
      logLevel .= k
      rowCapacity .= c
      iterations .= i
      workers .= j
      entropy .= listToMaybe (stdin <$ lst Seed)



compilerFlags :: [String] -> IO ([Flag], [String])
compilerFlags argv = case getOpt Permute args argv of
    (o, n, []) -> pure (o, n)
    (_, _, es) -> die $ concat es ++ usageInfo header args

