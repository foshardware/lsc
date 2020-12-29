-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE OverloadedStrings #-}

module LSC.SVG where

import Control.Applicative
import Control.Lens

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as Lazy
import Data.Text.Lazy.Builder (Builder, fromText)
import Data.Text.Lazy.Builder.Int

import Text.Blaze.Svg11 ((!), toSvg, toValue)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.Text (renderSvg)

import LSC.Component
import LSC.NetGraph
import LSC.Types



pixelsPerMicron :: Int
pixelsPerMicron = 60



gateColor :: Gate -> Arg
gateColor g | g ^. feedthrough = "lightyellow"
gateColor g | g ^. fixed = "lightblue"
gateColor _ = "lightgrey"



type Scale = Double

zoomOut :: Scale -> Int -> Int
zoomOut scale | scale > 0 = round . (/ scale) . fromIntegral
zoomOut scale = error $ "zoomOut: invalid scaling factor " ++ show scale



type Marker = Line Int

type Area = Component Layer Int


type Svg = S.Svg

type Arg = S.AttributeValue

type Args = (Arg, Arg)



plotStdout :: Scale -> NetGraph -> IO ()
plotStdout scale = Lazy.putStr . renderSvg . plot scale



plot :: Scale -> NetGraph -> Svg
plot scale = svgDoc . componentMap pixelated . liftA2 componentMap quadrantI id
  where 
     pixelated
       = fmap
       $ zoomOut scale
       . (* pixelsPerMicron)
     quadrantI
       = liftA2 (.) (moveX . abs . min 0 . view l) (moveY . abs . min 0 . view b)
       . netGraphArea



svgDoc :: NetGraph -> Svg
svgDoc top = S.docTypeSvg
  ! A.version "1.1"
  ! A.width  (toValue $ netGraphArea top ^. r)
  ! A.height (toValue $ netGraphArea top ^. t)
  $ do
    place `mapM_` view gates top
    route `mapM_` view nets top
    drawA ("black", "lightyellow") `mapM_` outerRim top



place :: Gate -> Svg
place g = do

    let a = g ^. space

    let x = a ^. l + height a `div` 32
        y = a ^. b + height a `div` 24
        k = height a `div` 9

    drawA ("black", gateColor g) a

    S.text_
      ! A.x (toValue x)
      ! A.y (toValue y)
      ! A.fontSize (toValue k)
      ! A.fontFamily "monospace"
      ! A.transform (toValue $ "rotate(90 " <> decimal x <> "," <> decimal y <> ")")
      $ toSvg $ views number decimal g <> ": " <> views identifier (forShort 8) g



route :: Net -> Svg
route _ = do
    pure ()



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



forShort :: Int -> Text -> Builder
forShort n string
    | T.length string > n
    = fromText (T.take (n - 2) string) <> ".."
forShort _ string
    = fromText string

