{-# LANGUAGE OverloadedStrings #-}

module LSC.SVG where

import Data.Foldable
import Data.String
import Data.Map (assocs)
import Data.Text hiding (take)
import qualified Data.Vector as V
import qualified Data.Text as Text
import qualified Data.Text.Lazy    as Lazy
import qualified Data.Text.Lazy.IO as Lazy

import Text.Blaze.Svg11 ((!), mkPath, m, l, z)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.Text (renderSvg)

import LSC.Types


type Circuit = Circuit2D [Arboresence]

type Options = (S.AttributeValue, S.AttributeValue)


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

  follow ("black", "transparent") path

place _ = pure ()


route :: Arboresence -> S.Svg
route (_, pins, paths) = do
  follow ("blue", "blue") `mapM_` paths
  follow ("black", "blue") pins


follow :: Options -> Path -> S.Svg
follow (stroke, fill) (Rect (left, bottom) (right, top) : xs) = do

  S.path
    ! A.d (mkPath $ m left bottom *> l left top *> l right top *> l right bottom *> z)
    ! A.stroke stroke
    ! A.fill fill
    ! A.strokeWidth "4"

  follow (stroke, fill) xs

follow _ _ = pure ()


svgPaths :: NetGraph -> Circuit
svgPaths netlist = Circuit2D

  [ (gate, gatePath gate)
  | gate <- toList $ gateVector netlist
  ]

  [ (net, inducePins =<< assocs (contacts net), netPaths net)
  | net <- toList $ netMapping netlist
  ]

  where

    inducePins (i, pins) =
      [ Rect (l' + x, b + y) (r + x, t + y)
      | pin <- pins
      , Rect (x, y) _ <- take 1 $ gatePath $ gateVector netlist V.! gateIndex i
      , Rect (l', b) (r, t) <- take 1 $ portRects $ pinPort pin
      ]


scaleDown :: Integer -> Circuit -> Circuit
scaleDown n (Circuit2D nodes edges) = Circuit2D

  [ (gate, f (`div` n) path)
  | (gate, path) <- nodes
  , let f = fmap . fmap
  ]

  [ (net, g (`div` n) pins, f (`div` n) paths)
  | (net, pins, paths) <- edges
  , let f = fmap . fmap . fmap
  , let g = fmap . fmap
  ]


renderText :: Text -> S.Svg
renderText = fromString . Text.unpack

