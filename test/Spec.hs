-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later


import Data.List (isPrefixOf)

import System.Environment

import Test.Tasty

import Spec.LSC
import Spec.LSC.Entropy
import Spec.LSC.FastDP
import Spec.LSC.FIFO
import Spec.LSC.FM
import Spec.LSC.KGGGP
import Spec.LSC.Legalize
import Spec.LSC.Types
import Spec.LSC.UnionFind



main :: IO ()
main = defaultMain . lsc =<< getArgs


lsc :: [String] -> TestTree
lsc args = testGroup "LSC" $
  [ types
  , fifo
  , unionFind
  , entropy
  , gggp
  , fm
  , legalize
  , fastdp
  ] ++ [ concurrency | isPrefixOf "-j" `any` args ]

