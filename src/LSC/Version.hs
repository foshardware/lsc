-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE TemplateHaskell #-}

module LSC.Version where

import Data.Char
import Data.FileEmbed
import Distribution.PackageDescription.TH

versionString :: String
versionString = $(packageVariable package) ++ ", commit "++ commitString

commitString :: String
commitString = reverse . dropWhile isSpace . reverse $
  if "ref:" == take 4 $(embedStringFile ".git/HEAD")
    then $(embedStringFile ".git/refs/heads/master")
    else $(embedStringFile ".git/HEAD")
