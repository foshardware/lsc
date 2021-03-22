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


workTreeExceptions :: [String]
workTreeExceptions =
  [ ""
  , "cabal build"
  , "dirty"
  ]



main :: IO ()
main
  = do

    when (commitString `elem` workTreeExceptions)
      $ die $ "commit string is <" ++ commitString ++">, do you have outstanding changes?"

    paths <- getFilesRecursive "src"

    haddock $
      [ "--html"
      , "--odir=./doc/dist/latest"
      , "--source-entity-line=" ++ sourceBaseUrl ++ "/%F" ++ lineFormatString
      ] ++ [ path | (path, ext) <- splitExtension <$> paths, ext `elem` sourceExtensions ]

