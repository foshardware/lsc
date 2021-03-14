-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.LSC.GlobalRouting where

import Control.Applicative
import Control.Lens
import Control.Monad.ST

import Data.Default
import Data.Foldable
import Data.HashMap.Lazy (insert, adjust, keys, elems)
import Data.Vector (update, fromListN, (!))
import qualified Data.Vector as Vector

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import LSC.Component
import LSC.GlobalRouting
import LSC.Legalize
import LSC.NetGraph
import LSC.Polygon
import LSC.Types

import Spec.LSC.Types



standardPins :: Int
standardPins = 20



globalRouting :: TestTree
globalRouting = testGroup "GlobalRouting"
  []



newtype GlobalRouting a = GlobalRouting a


instance Arbitrary (GlobalRouting NetGraph) where

    arbitrary = do

        k <- choose (80, 140)

        Given new <- arbitrary
        let top = new & gates %~ Vector.take k

        grp <- pure $ runST $ liftA2 mapM rowLegalization (getRows . view gates) =<< rowJuggling 1 top
        let legal = rebuildEdges $ top & gates %~ flip update (liftA2 (,) (view number) id <$> fold grp)

        ps <- fromListN standardPins <$> vector standardPins

        pure
          $ GlobalRouting
          $ legal &~ do
            nets %= fmap (over contacts $ imap $ \ i ->
                map $ set geometry $ locatePin (view gates legal ! i) $ say $ ps ! mod i standardPins)
            where

            locatePin :: Gate -> Pin -> [Port]
            locatePin g = map (bimap (+ g ^. space . l) (+ g ^. space . b)) . view geometry




feedthroughs :: TestTree
feedthroughs = testGroup "Determine feedthroughs"

  [ testCase "Multiple application"
      $ do

        GlobalRouting top <- generate arbitrary

        top1 <- fmap wireFeedthroughs $ stToIO $ globalDetermineFeedthroughs top

        top2 <- fmap wireFeedthroughs $ stToIO $ globalDetermineFeedthroughs top1

        assertEqual "not identical: feedthroughs count"
          (views gates (length . Vector.filter (view feedthrough)) top1)
          (views gates (length . Vector.filter (view feedthrough)) top2)

        assertBool "not identical: length" $ views gates length top1 == views gates length top2

        assertBool "not identical: feedthroughs" $
             (views gates (Vector.filter (view feedthrough)) top1)
          == (views gates (Vector.filter (view feedthrough)) top2)

        assertBool "not identical" $ view gates top1 == view gates top2

  ] where

    wireFeedthroughs :: NetGraph -> NetGraph
    wireFeedthroughs top = top & nets %~ flip

        (foldl' (\ a (p, n, g, s) ->
            adjust (over contacts $ insert g [def & identifier .~ p & geometry .~ [simplePolygon s]]) n a))

        (((,,,) <$> head . keys . view wires <*> head . elems . view wires <*> view number <*> view space)
                <$> Vector.filter (view feedthrough) (top ^. gates))

