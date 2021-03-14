-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

module Spec.LSC.Polygon
  ( polygons 
  ) where

import Control.Lens

import Test.Tasty
import Test.Tasty.HUnit

import LSC.Component
import LSC.Polygon



polygons :: TestTree
polygons = testGroup "Polygon"
  [ testCase "PTR"
      $ do

        let g c = (c ^. l, c ^. b, width c, height c)

        let q = [(1,1), (1,2), (2,2), (2,3), (4,3), (4,4), (5,4), (5,1), (4,1), (4,2), (3,2), (3,1)]
            p = constructPolygon q :: Polygon' l Int

        let result = map g $ polygon p

        assertEqual "3 rectangles" [(1,1,2,1), (2,2,2,1), (4,1,1,3)] result
  ]

