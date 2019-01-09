{-# LANGUAGE OverloadedStrings #-}

module LSC.SVG where

import Data.Foldable
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


type Circuit = Stage2


plotStdout :: NetGraph -> IO ()
plotStdout = Lazy.putStr . plot


plot :: NetGraph -> Lazy.Text
plot = renderSvg . svgDoc . scaleDown 100 . svgPaths


svgDoc :: Circuit -> S.Svg
svgDoc (Circuit2D nodes edges) = S.docTypeSvg
  ! A.version "1.1"
  ! A.width "100000"
  ! A.height "100000"
  $ do
    place `mapM_` nodes
    route `mapM_` edges


place :: (Gate, Path) -> S.Svg
place (g, path@(Rect (x, y) _ : _)) = do

  S.text_
    ! A.x (S.toValue $ x + 42)
    ! A.y (S.toValue $ y + 24)
    ! A.fontSize "24"
    ! A.fontFamily "monospace"
    ! A.transform (fromString $ "rotate(90 "++ show (x + 8) ++","++ show (y + 24)  ++")")
    $ renderText $ gateIdent g

  follow path

place _ = pure ()


route :: (Net, [Path]) -> S.Svg
route (net, p : ps) = follow p *> route (net, ps)
route _ = pure ()


follow :: Path -> S.Svg
follow (Rect (left, bottom) (right, top) : xs) = do

  S.path
    ! A.d (mkPath $ m left bottom *> l left top *> l right top *> l right bottom *> z)
    ! A.stroke "black"
    ! A.fill "transparent"
    ! A.strokeWidth "4"

  follow xs

follow _ = pure ()


svgPaths :: NetGraph -> Circuit
svgPaths netlist = Circuit2D

  [ (gate, gatePath gate)
  | gate <- toList $ gateVector netlist
  ]

  [ (net, netPaths net)
  | net <- toList $ netMapping netlist
  ]


scaleDown :: Integer -> Circuit -> Circuit
scaleDown n (Circuit2D nodes edges) = Circuit2D

  [ ( gate
    , [ Rect (div x1 n, div y1 n) (div x2 n, div y2 n)
      | Rect (x1, y1) (x2, y2) <- path
      ]
    )
  | (gate, path) <- nodes
  ]

  [ ( net
    , [ [ Rect (div x1 n, div y1 n) (div x2 n, div y2 n)
        | Rect (x1, y1) (x2, y2) <- path
        ]
      | path <- paths
      ]
    )
  | (net, paths) <- edges
  ]


renderText :: Text -> S.Svg
renderText = fromString . Text.unpack

