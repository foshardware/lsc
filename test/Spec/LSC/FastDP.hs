-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.LSC.FastDP
    ( fastdp
    , layout
    ) where

import Control.Applicative
import Control.Lens
import Control.Monad.ST
import Data.Default
import Data.Either
import Data.Foldable
import Data.IntMap (findMax)
import qualified Data.IntMap as IntMap
import Data.Vector (accum, replicate, update)

import Prelude hiding (replicate)

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import LSC.Component
import LSC.FastDP
import LSC.Legalize
import LSC.NetGraph
import LSC.Types

import Spec.LSC.Types



fastdp :: TestTree
fastdp = testGroup "FastDP"
  [ layouts
  , localReorders
  , sscs
  ]



newtype FastDP a = FastDP { unFastDP :: a }


instance Arbitrary (FastDP NetGraph) where
    arbitrary = do
        Given top <- arbitrary
        grp <- pure $ runST $ liftA2 mapM rowLegalization (getRows . view gates) =<< rowJuggling 1 top
        pure
          $ FastDP
          $ rebuildEdges
          $ top &~ do
            gates %= flip update (liftA2 (,) (view number) id <$> fold grp)


instance Arbitrary (FastDP Layout) where
    arbitrary = do
        Given top <- arbitrary
        grp <- pure $ runST $ liftA2 mapM rowLegalization (getRows . view gates) =<< rowJuggling 1 top
        pure
          $ FastDP
          $ buildLayout
          $ fold grp


layout :: Iso' (FastDP NetGraph) (FastDP Layout)
layout = iso
    (FastDP . buildLayout . view gates . unFastDP)
    (FastDP . rebuildEdges . flip (set gates) def . gateVector . unFastDP)
    where
      gateVector
        = liftA2 (accum (flip const))
          (flip replicate def . sum . fmap (sum . fmap length))
          (map (liftA2 (,) (view number) id) . foldMap (rights . toList))



instance Arbitrary (FastDP Segment) where
    arbitrary = do
        Given top <- arbitrary
        let jug = runST $ rowJuggling 1 top
        k <- choose (0, pred . length $ views gates getRows jug)
        let row = runST $ rowLegalization jug $ views gates getRows jug !! k
        pure
          $ FastDP
          $ intersperseSpace
          $ foldl' (flip insertGate) mempty
          $ row




layouts :: TestTree
layouts = testGroup "Layout"
  [ layoutIsos
  , segmentIterators
  , intersperseSpaces
  , cutLayouts
  ]



layoutIsos :: TestTree
layoutIsos = testGroup "NetGraph isomorphism"

  [ testCase "to . from"
      $ do

        FastDP top <- generate arbitrary
        let FastDP nxt = FastDP top ^. layout . from layout

        assertEqual "gates lost"
          (top ^. gates . to length)
          (nxt ^. gates . to length)
        assertEqual "gate information lost"
          (hypothenuse $ boundingBox $ view space <$> top ^. gates)
          (hypothenuse $ boundingBox $ view space <$> nxt ^. gates)
        assertBool "gate information lost" $
             (view space <$> top ^. gates)
          == (view space <$> nxt ^. gates)

        assertEqual "nets lost"
          (top ^. nets . to length)
          (nxt ^. nets . to length)
        assertEqual "net information lost"
          (hypothenuse $ boundingBox $ foldMap' (view space) . view members <$> top ^. nets)
          (hypothenuse $ boundingBox $ foldMap' (view space) . view members <$> nxt ^. nets)
        assertBool "net information lost" $
             (foldMap' (view space) . view members <$> top ^. nets)
          == (foldMap' (view space) . view members <$> nxt ^. nets)

  , testCase "from . to"
      $ do

        FastDP lay <- generate arbitrary
        let FastDP nxt = FastDP lay ^. from layout . layout

        assertEqual "gates lost"
          (sum $ length . rights . toList <$> lay)
          (sum $ length . rights . toList <$> nxt)
        assertEqual "information lost"
          (hypothenuse $ boundingBox $ foldMap' (either id (view space)) <$> lay)
          (hypothenuse $ boundingBox $ foldMap' (either id (view space)) <$> nxt)
        assertBool "information lost" $
             (fmap (fmap (view space)) <$> lay)
          == (fmap (fmap (view space)) <$> nxt)
  ]



segmentIterators :: TestTree
segmentIterators = testGroup "Segment iterators"

  [ testCase "To the left"
      $ sequence_
      $ replicate 20
      $ do

        FastDP seg <- generate arbitrary

        let n = fst . findMax $ seg

        k <- generate $ choose (0, n)

        let xs = reverse . map snd . leftNext seg $ k
        let ys = toList . fst . IntMap.split k $ seg

        assertEqual "occupants"
          (either (const (-1)) (view number) <$> ys)
          (either (const (-1)) (view number) <$> xs)
        assertBool "occupied area"
          $ map (view space <$>) xs == map (view space <$>) ys

  , testCase "To the right"
      $ sequence_
      $ replicate 20
      $ do

        FastDP seg <- generate arbitrary

        let n = fst . findMax $ seg

        k <- generate $ choose (0, n)

        let xs = map snd . rightNext seg $ k
        let ys = toList . snd . IntMap.split k $ seg

        assertEqual "occupants"
          (either (const (-1)) (view number) <$> ys)
          (either (const (-1)) (view number) <$> xs)
        assertBool "occupied area"
          $ map (view space <$>) xs == map (view space <$>) ys

  , testCase "Spaces"
      $ sequence_
      $ replicate 20
      $ do

        FastDP seg <- generate arbitrary
        let n = fst . findMax $ seg

        k <- generate $ choose (0, n)

        let xs = spaces rightNext k seg $ mempty & l .~ 0 & r .~ 0
            ys = reverse $ spaces leftNext k seg $ mempty & l .~ n & r .~ n

        let zs = lefts . toList $ seg

        assertEqual "spaces to the right"
          (view l <$> zs)
          (view l <$> xs)
        assertEqual "spaces to the left"
          (view l <$> zs)
          (view l <$> ys)

        assertBool "occupied area"
          $ zip xs ys == zip zs zs
  ]



intersperseSpaces :: TestTree
intersperseSpaces = testGroup "Intersperse spaces"
  [ testCase "Into segment"
      $ sequence_
      $ replicate 10
      $ do

        FastDP lay <- generate arbitrary

        let nxt = fmap (intersperseSpace . IntMap.filter isRight) lay :: Layout

        assertBool "alternating" $ and
          [ isLeft g && isRight h || isLeft h && isRight g
          | segment <- toList nxt
          , (g, h) <- toList segment `zip` tail (toList segment)
          ]

        assertBool "free space" $ and
          [ a ^. r == h ^. l || a ^. l == h ^. r
          | segment <- toList nxt
          , (g, g') <- toList segment `zip` tail (toList segment)
          , a <- lefts [g, g']
          , h <- view space <$> rights [g, g']
          ]
  ]



cutLayouts :: TestTree
cutLayouts = testGroup "Cut layout"

  [ testCase "Layout"
      $ sequence_
      $ replicate 10
      $ do

        FastDP lay <- generate arbitrary
        let n = fst . findMax $ lay

        j <- generate $ choose (0, n)
        k <- generate $ choose (0, n)

        let cut = cutLayout (min j k, max j k) lay :: Layout
        let area = foldMap' (foldMap' (either implode (implode . view space))) cut

        assertBool "top"     $ max j k >= area ^. t
        assertBool "bottom"  $ min j k <= area ^. b

  , testCase "Segment"
      $ sequence_ 
      $ replicate 40
      $ do

        FastDP seg <- generate arbitrary
        let n = fst . findMax $ seg

        j <- generate $ choose (0, n)
        k <- generate $ choose (0, n)

        let cut = cutLayout (min j k, max j k) seg :: Segment
        let area = foldMap' (either implode (implode . view space)) cut

        assertBool "left"  $ min j k <= area ^. l
        assertBool "right" $ max j k >= area ^. r
  ]




localReorders :: TestTree
localReorders = testGroup "Local reorder segment"

  [ testCase "Order 1"
      $ do
        FastDP top <- generate arbitrary
        reordered <- stToIO $ localReorderSegment 1 top `mapM` views gates getSegments top
        assertBool "not identical" $ reordered == views gates getSegments top
  ]




sscs :: TestTree
sscs = testGroup "Single segment clustering"
  [
  ]

