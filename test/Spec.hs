-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

import Control.Concurrent
import Data.Foldable
import Data.List
import Data.Maybe

import Text.Parsec (parse)
import Text.ParserCombinators.Parsec.Number (decimal)

import System.Environment

import Test.Tasty

import LSC.Version

import Spec.LSC
import Spec.LSC.Entropy
import Spec.LSC.FastDP
import Spec.LSC.FIFO
import Spec.LSC.FM
import Spec.LSC.GlobalRouting
import Spec.LSC.KGGGP
import Spec.LSC.Legalize
import Spec.LSC.SegmentTree
import Spec.LSC.Types
import Spec.LSC.UnionFind



data TestArgs = TestArgs
  { _j :: Maybe Int
  }


main :: IO ()
main = do

    args <- getArgs

    j <- mapM (either (fail . show) pure . parse decimal "-j" . drop 2) (isPrefixOf "-j" `find` args)
    mapM_ setNumCapabilities j

    putStrLn []
    defaultMain $ lsc TestArgs
      { _j = j
      }



lsc :: TestArgs -> TestTree
lsc opts = testGroup versionString $
  [ types
  , fifo
  , segmentTree
  , unionFind
  , entropy
  , gggp
  , fm
  , legalize
  , fastdp
  , globalRouting
  ] ++ [ concurrency | isJust $ _j opts ]

