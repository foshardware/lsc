{-# LANGUAGE OverloadedStrings #-}

module LSC.SVG where

import Data.Text.Lazy
import qualified Data.Text.Lazy.IO as Text

import Text.Blaze.Svg11 ((!), mkPath, m, h, v, z)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.Text (renderSvg)

import LSC.Types

plotStdout :: Circuit2D -> IO ()
plotStdout = Text.putStr . plot

plot :: Circuit2D -> Text
plot = renderSvg . svgDoc . scaleDown 100

svgDoc :: Circuit2D -> S.Svg
svgDoc (Circuit2D nodes edges) = S.docTypeSvg
  ! A.version "1.1"
  ! A.width "10000"
  ! A.height "10000"
  $ do
    mapM_ rect nodes

rect :: Rectangle -> S.Svg
rect (x, y, width, height) = S.path
  ! A.d (mkPath $ m x y *> h (x + width) *> v (y + height) *> h x *> z)
  ! A.stroke "black"
  ! A.fill "transparent"
  ! A.strokeWidth "4"

scaleDown :: Integer -> Circuit2D -> Circuit2D
scaleDown n (Circuit2D nodes edges) = Circuit2D
  [ (div a n, div b n, div c n, div d n)
  | (a, b, c, d) <- nodes
  ]
  edges
