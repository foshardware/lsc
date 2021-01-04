-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE TemplateHaskell #-}

module LSC.Version where

import Data.FileEmbed
import Distribution.PackageDescription.TH



versionString :: String
versionString = $(packageVariable package) ++ ", " ++ commitString


commitString :: String
commitString = $(embedStringFile =<< makeRelativeToProject ".status")

