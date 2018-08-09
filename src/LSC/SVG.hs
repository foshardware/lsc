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
  ! A.width "100000"
  ! A.height "100000"
  $ do
    mapM_ place  nodes
    mapM_ follow edges

place :: Rectangle -> S.Svg
place (x, y, width, height) = S.path
  ! A.d (mkPath $ m x y *> h (x + width) *> v (y + height) *> h x *> z)
  ! A.stroke "black"
  ! A.fill "transparent"
  ! A.strokeWidth "4"

follow :: Path -> S.Svg
follow (Path ((x1, y1) : (x2, y2) : xs)) = do

  S.line
    ! A.x1 (S.toValue x1)
    ! A.y1 (S.toValue y1)
    ! A.x2 (S.toValue x2)
    ! A.y2 (S.toValue y2)
    ! A.stroke "black"
    ! A.fill "transparent"
    ! A.strokeWidth "3"

  follow (Path xs)

follow _ = pure ()


scaleDown :: Integer -> Circuit2D -> Circuit2D
scaleDown n (Circuit2D nodes edges) = Circuit2D

  [ (div x n, div y n, div w' n, div h' n)
  | (x, y, w', h') <- nodes
  ]

  [ Path [ (div x n, div y n) | (x, y) <- pos ]
  | Path pos <- edges
  ]
