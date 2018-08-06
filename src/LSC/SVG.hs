{-# LANGUAGE OverloadedStrings #-}

module LSC.SVG where

import Data.Text.Lazy
import qualified Data.Text.Lazy.IO as Text

import Text.Blaze.Svg11 ((!), mkPath, rotate, l, m, h, v, z)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.Text (renderSvg)

import LSC.Types

plotStdout :: Circuit2D -> IO ()
plotStdout = Text.putStr . plot

plot :: Circuit2D -> Text
plot = renderSvg . svgDoc

svgDoc :: Circuit2D -> S.Svg
svgDoc cs = S.docTypeSvg
  ! A.version "1.1"
  ! A.width "1500"
  ! A.height "10000"
  $ do
    mapM_ rect cs

rect :: Rectangle -> S.Svg
rect (x', y', width', height') = S.path
  ! A.d (mkPath $ m x y *> h (x + width) *> v (y + height) *> h x *> z)
  ! A.stroke "black"
  ! A.fill "transparent"
  ! A.strokeWidth "2"
  where x = div x' 1000
        y = div y' 1000
        width = div width' 1000
        height = div height' 1000