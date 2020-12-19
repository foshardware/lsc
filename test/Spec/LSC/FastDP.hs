-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

module Spec.LSC.FastDP
    ( fastdp
    ) where

import Test.Tasty
import Test.Tasty.HUnit
-- import Test.Tasty.QuickCheck

-- import LSC.FastDP


fastdp :: TestTree
fastdp = testGroup "FastDP"
  [ ssc
  ]


ssc :: TestTree
ssc = testGroup "Single segment clustering"
  [ testCase "Center cluster"
      $ do
        pure ()
  ]
