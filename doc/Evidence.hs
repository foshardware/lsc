-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later


import Control.Monad

import Documentation.Haddock

import System.Directory.Recursive
import System.Exit
import System.FilePath

import LSC.Version



sourceBaseUrl :: String
sourceBaseUrl = "https://github.com/foshardware/lsc/tree/" ++ commitString


lineFormatString :: String
lineFormatString = "#L%L"


sourceExtensions :: [String]
sourceExtensions =
  [ ".hs"
  , ".lhs"
  ]



main :: IO ()
main
  = do

    when (commitString == "dirty1")
      $ die $ "Work tree is dirty, commit your changes first."

    when (commitString `elem` ["", "not found"])
      $ die $ "Work tree absent."

    paths <- getFilesRecursive "src"

    haddock $
      [ "--html"
      , "--odir=./doc/dist/latest"
      , "--source-entity-line=" ++ sourceBaseUrl ++ "/%F" ++ lineFormatString
      ] ++ [ path | (path, ext) <- splitExtension <$> paths, ext `elem` sourceExtensions ]

