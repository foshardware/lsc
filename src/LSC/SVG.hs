{-# LANGUAGE OverloadedStrings #-}

module LSC.SVG where

import Data.String
import Data.Text
import qualified Data.Text as Text
import qualified Data.Text.Lazy    as Lazy
import qualified Data.Text.Lazy.IO as Lazy

import Text.Blaze.Svg11 ((!), mkPath, m, h, v, z)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.Text (renderSvg)

import LSC.Types



plotStdout :: Circuit2D -> IO ()
plotStdout = Lazy.putStr . plot


plot :: Circuit2D -> Lazy.Text
plot = renderSvg . svgDoc . scaleDown 100


svgDoc :: Circuit2D -> S.Svg
svgDoc (Circuit2D nodes edges) = S.docTypeSvg
  ! A.version "1.1"
  ! A.width "100000"
  ! A.height "100000"
  $ do
    mapM_ place nodes
    mapM_ follow $ snd <$> edges


place :: (Gate, Path) -> S.Svg
place (g, path) = do

  follow path


follow :: Path -> S.Svg
follow (Path ((x1, y1) : (x2, y2) : xs)) = do

  S.line
    ! A.x1 (S.toValue x1)
    ! A.y1 (S.toValue y1)
    ! A.x2 (S.toValue x2)
    ! A.y2 (S.toValue y2)
    ! A.stroke "darkblue"
    ! A.fill "transparent"
    ! A.strokeWidth "3"

  follow (Path ((x2, y2) : xs))

follow _ = pure ()


scaleDown :: Integer -> Circuit2D -> Circuit2D
scaleDown n (Circuit2D nodes edges) = Circuit2D

  [ (gate, Path [ (div x n, div y n) | (x, y) <- pos ])
  | (gate, Path pos) <- nodes
  ]

  [ (net, Path [ (div x n, div y n) | (x, y) <- pos ])
  | (net, Path pos) <- edges
  ]


renderText :: Text -> S.Svg
renderText = fromString . Text.unpack

