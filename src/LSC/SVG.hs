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



plotStdout :: Circuit2D Steiner -> IO ()
plotStdout = Lazy.putStr . plot


plot :: Circuit2D Steiner -> Lazy.Text
plot = renderSvg . svgDoc . scaleDown 100


svgDoc :: Circuit2D Steiner -> S.Svg
svgDoc (Circuit2D nodes edges steiner) = S.docTypeSvg
  ! A.version "1.1"
  ! A.width "100000"
  ! A.height "100000"
  $ do
    arbor `mapM_` steiner
    place `mapM_` nodes
    follow False `mapM_` fmap snd edges


arbor :: [(Integer, Integer)] -> S.Svg
arbor xs = sequence_
  [ S.circle
    ! A.cx (S.toValue x)
    ! A.cy (S.toValue y)
    ! A.r "5"
    ! A.stroke "black"
    ! A.fill "transparent"
    ! A.strokeWidth "4"
       
  | (x, y) <- xs
  ]


place :: (Gate, Path) -> S.Svg
place (g, path@(Path ((x, y) : _))) = do

  S.text_
    ! A.x (S.toValue $ x + 42)
    ! A.y (S.toValue $ y + 24)
    ! A.fontSize "24"
    ! A.fontFamily "monospace"
    ! A.transform (fromString $ "rotate(90 "++ show (x + 8) ++","++ show (y + 24)  ++")")
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


scaleDown :: Integer -> Circuit2D Steiner -> Circuit2D Steiner
scaleDown n (Circuit2D nodes edges steiner) = Circuit2D

  [ (gate, Path [ (div x n, div y n) | (x, y) <- pos ])
  | (gate, Path pos) <- nodes
  ]

  [ (net, Path [ (div x n, div y n) | (x, y) <- pos ])
  | (net, Path pos) <- edges
  ]

  (fmap ( \ pos -> [ (div x n, div y n) | (x, y) <- pos ]) steiner)


renderText :: Text -> S.Svg
renderText = fromString . Text.unpack

