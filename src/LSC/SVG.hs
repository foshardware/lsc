-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module LSC.SVG where

#if !MIN_VERSION_base(4,10,0)
import Data.Semigroup ((<>))
#endif

import Control.Applicative
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
  = svgDoc . region pixelated pixelated . liftA2 id quadrantI id
  where
    pixelated
      = round . (/ scale)
      . fromIntegral . (* pixelsPerMicron)
    quadrantI
      = liftA2 region ((+) . abs . min 0 . view l) ((+) . abs . min 0 . view b)
      . netGraphArea



svgDoc :: NetGraph -> Svg
svgDoc top = S.docTypeSvg
  ! A.version "1.1"
  ! A.width  (toValue $ area ^. r)
  ! A.height (toValue $ area ^. t)
  $ do
    track area `mapM_` view (supercell . tracks) top
    place `mapM_` view gates top
    route `mapM_` view nets top
    ports `mapM_` view nets top
    drawA ("black", "lightyellow") `mapM_` outerRim top
  where
    area = netGraphArea top



place :: Gate -> Svg
place g = do

    let a = g ^. space

    let d = rotation $ a ^. orientation
        (x, y) = label a

    let k = height a `div` 9

    drawA ("black", gateColor g) a

    S.text_
      ! A.x (toValue x)
      ! A.y (toValue y)
      ! A.fontSize (toValue k)
      ! A.fontFamily "monospace"
      ! A.transform (toValue $ "rotate(" <> decimal d <> " " <> decimal x <> "," <> decimal y <> ")")
      $ toSvg $ views number decimal g <> ": " <> views identifier (forShort 8) g


label :: Area -> (Int, Int)
label a = case a ^. orientation of
    FN -> (a ^. r - height a `div` 32, a ^. t - height a `div` 24)
    _  -> (a ^. l + height a `div` 32, a ^. b + height a `div` 24)


rotation :: Orientation -> Int
rotation FN = 270
rotation  _ = 90



route :: Net -> Svg
route n
  | views geometry null n
  = do
    drawL (views identifier identColor n) `mapM_` view netSegments n

route _
  = do
    pure ()



track :: Area -> Either Track Track -> Svg
track a (Right x)
  = for_ (x ^. stabs . to elems)
  $ drawL (x ^. layers z . to layerColor) . vertical (a ^. t)
track a (Left y)
  = for_ (y ^. stabs . to elems)
  $ drawL (y ^. layers z . to layerColor) . horizontal (a ^. r)


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
drawA (border, background) a = S.rect
  ! A.x (toValue $ a ^. l)
  ! A.y (toValue $ a ^. b)
  ! A.width (toValue $ width a)
  ! A.height (toValue $ height a)
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

