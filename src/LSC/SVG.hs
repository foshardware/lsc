-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE OverloadedStrings #-}

module LSC.SVG where

import Control.Lens
import Data.Foldable
import Data.Hashable
import Data.IntSet (elems)
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as Lazy
import Data.Text.Lazy.Builder (Builder, fromText)
import Data.Text.Lazy.Builder.Int

import Text.Blaze.Svg11 ((!), toSvg, toValue)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.Text (renderSvg)

import LSC.Cartesian
import LSC.Component
import LSC.Model
import LSC.NetGraph
import LSC.Polygon



pixelsPerMicron :: Int
pixelsPerMicron = 60



gateColor :: Gate -> Arg
gateColor g | g ^. feedthrough = "lightyellow"
gateColor g | g ^. fixed = "lightblue"
gateColor _ = "lightgrey"


layerColor :: [Layer] -> Arg
layerColor [Metal1] = "#EFEFFF"
layerColor _ = "transparent"


identColor :: Identifier -> Arg
identColor
  = toValue
  . T.cons '#'
  . T.justifyRight 6 '0'
  . base16Identifier
  . flip mod 0x666666
  . hash



type Marker = Line' Int

type Area = Component' Layer Int

type Poly = Polygon' Layer Int


type Svg = S.Svg

type Arg = S.AttributeValue

type Args = (Arg, Arg)



plotStdout :: Double -> NetGraph -> IO ()
plotStdout scale = Lazy.putStr . renderSvg . plot scale



plot :: Double -> NetGraph -> Svg
plot scale
  | scale <= 0
  = error $ "plot: invalid scaling factor " ++ show scale
plot scale
  = svgDoc
  . region pixelated pixelated
  . (quadrantI <*> id)
  where
    pixelated
      = round . (/ scale)
      . fromIntegral . (* pixelsPerMicron)
    quadrantI
      = (region <$> (+) . abs . min 0 . view l <*> (+) . abs . min 0 . view b)
      . netGraphArea



svgDoc :: NetGraph -> Svg
svgDoc top = S.docTypeSvg
  ! A.version "1.1"
  ! A.width  (views r toValue area)
  ! A.height (views t toValue area)
  $ do
    track area `mapM_` view (supercell . tracks) top
    place `mapM_` view gates top
    route `mapM_` view nets top
    ports `mapM_` view nets top
    drawA ("black", "lightyellow") `mapM_` outerRim top
  where
    area = netGraphArea top



place :: Gate -> Svg
place gate = do

    let area = gate ^. geometry

    let (x, y) = label area
        d = area ^. transformation . to rotate
        k = height area `div` 9

    drawA ("black", gateColor gate) area

    S.text_
      ! A.x (toValue x)
      ! A.y (toValue y)
      ! A.fontSize (toValue k)
      ! A.fontFamily "monospace"
      ! A.transform (toValue $ "rotate(" <> decimal d <> " " <> decimal x <> "," <> decimal y <> ")")
      $ toSvg $ views number decimal gate <> ": " <> views identifier (forShort 8) gate


label :: Area -> (Int, Int)
label area = case area ^. transformation of
    FN -> (area ^. r - height area `div` 32, area ^. t - height area `div` 24)
    _  -> (area ^. l + height area `div` 32, area ^. b + height area `div` 24)


rotate :: Orientation -> Int
rotate FN = 270
rotate  _ = 90



route :: Net -> Svg
route net
  | views geometry null net
  = do
    drawL (views identifier identColor net) `mapM_` view netSegments net

route _
  = do
    pure ()



track :: Area -> Either Track Track -> Svg
track area (Right x)
  = for_ (x ^. stabs . to elems)
  $ drawL (x ^. layers z . to layerColor) . vertical (area ^. t)
track area (Left y)
  = for_ (y ^. stabs . to elems)
  $ drawL (y ^. layers z . to layerColor) . horizontal (area ^. r)


horizontal, vertical :: Int -> Int -> Marker
horizontal x y = Line (0, y) (x, y)
vertical   y x = Line (x, 0) (x, y)



ports :: Net -> Svg
ports
  = mapM_ drawP
  . foldMap (view geometry)
  . fold
  . view contacts



drawP :: Poly -> Svg
drawP p = S.polygon
  ! A.points (toValue $ fold $ intersperse " " points)
  ! A.fill "transparent"
  ! A.stroke "black"
  where
    points = toList $ p ^. path <&> \ (x, y) -> decimal x <> "," <> decimal y



drawA :: Args -> Area -> Svg
drawA (border, background) area = S.rect
  ! A.x (views l toValue area)
  ! A.y (views b toValue area)
  ! A.width (toValue $ width area)
  ! A.height (toValue $ height area)
  ! A.stroke border
  ! A.fill background



drawL :: Arg -> Marker -> Svg
drawL color (Line (x1, y1) (x2, y2)) = S.line
  ! A.x1 (toValue x1)
  ! A.y1 (toValue y1)
  ! A.x2 (toValue x2)
  ! A.y2 (toValue y2)
  ! A.stroke color
  ! A.strokeWidth "3"



forShort :: Int -> Text -> Builder
forShort n string
  | T.length string > n
  = fromText (T.take (n - 2) string) <> ".."
forShort _ string
  = fromText string

