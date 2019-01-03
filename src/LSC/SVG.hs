{-# LANGUAGE OverloadedStrings #-}

module LSC.SVG where

import Data.String
import Data.Text
import qualified Data.Text as Text
import qualified Data.Text.Lazy    as Lazy
import qualified Data.Text.Lazy.IO as Lazy

import Text.Blaze.Svg11 ((!), mkPath, m, l, z)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.Text (renderSvg)

import LSC.Types



plotStdout :: Stage1 -> IO ()
plotStdout = Lazy.putStr . plot


plot :: Stage1 -> Lazy.Text
plot = renderSvg . svgDoc . svgPaths . scaleDown 100


svgDoc :: Stage1 -> S.Svg
svgDoc (Circuit2D nodes steiner) = S.docTypeSvg
  ! A.version "1.1"
  ! A.width "100000"
  ! A.height "100000"
  $ do
    uncurry arbor `mapM_` steiner
    place `mapM_` nodes


arbor :: (Net, Identifier) -> Path -> S.Svg
arbor _ path = follow False path


place :: (Gate, Path) -> S.Svg
place (g, path@(Path ((x, y) : _))) = do

  S.text_
    ! A.x (S.toValue $ x + 42)
    ! A.y (S.toValue $ y + 24)
    ! A.fontSize "24"
    ! A.fontFamily "monospace"
    -- ! A.transform (fromString $ "rotate(90 "++ show (x + 8) ++","++ show (y + 24)  ++")")
    $ renderText $ gateIdent g

  follow True path

place _ = pure ()


follow :: Bool -> Path -> S.Svg
follow finish (Path ((px, py) : xs)) = do

  let z' = if finish then z else pure ()

  S.path
    ! A.d (mkPath $ m px py *> sequence [ l x y | (x, y) <- xs ] *> z')
    ! A.stroke "black"
    ! A.fill "transparent"
    ! A.strokeWidth "4"

follow _ _ = pure ()


svgPaths :: Stage1 -> Stage1
svgPaths (Circuit2D nodes steiner) = Circuit2D

  [ (gate, Path $
     let (left, bottom) : (right, top) : _ = pos
     in [(left, bottom), (left, top), (right, top), (right, bottom)])
  | (gate, Path pos) <- nodes
  ]

  steiner


scaleDown :: Integer -> Stage1 -> Stage1
scaleDown n (Circuit2D nodes steiner) = Circuit2D

  [ (gate, Path [ (div x n, div y n) | (x, y) <- pos ])
  | (gate, Path pos) <- nodes
  ]

  [ (net, Path [ (div x n, div y n) | (x, y) <- pos ])
  | (net, Path pos) <- steiner
  ]


renderText :: Text -> S.Svg
renderText = fromString . Text.unpack

