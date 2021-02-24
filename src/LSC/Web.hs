-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

module LSC.Web where

import Data.Hashable

import LSC.Types


routeWeb :: NetGraph -> LSC NetGraph
routeWeb = fail . show . hash
