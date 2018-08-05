{-# LANGUAGE OverloadedStrings #-}

module LSC.SVG where

import Data.Text.Lazy

import Text.Blaze.Svg11 ((!), mkPath, rotate, l, m, h, v, z)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.Text (renderSvg)

plot :: Text
plot = renderSvg svgDoc

svgDoc :: S.Svg
svgDoc = S.docTypeSvg
  ! A.version "1.1"
  ! A.width "150"
  ! A.height "100" $ do
    rect (10, 20, 20, 70)

rect :: (Integer, Integer, Integer, Integer) -> S.Svg
rect (x, y, width, height) = S.path
  ! A.d (mkPath $ m x y *> h (x + width) *> v (y + height) *> h x *> z)
  ! A.stroke "black"
  ! A.fill "transparent"
  ! A.strokeWidth "10"
