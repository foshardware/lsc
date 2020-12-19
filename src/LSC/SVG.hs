-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE OverloadedStrings #-}

module LSC.SVG where

import Control.Lens

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as Lazy
import Data.Text.Lazy.Builder (Builder, fromText)
import Data.Text.Lazy.Builder.Int

import Text.Blaze.Svg11 ((!), mkPath, toSvg, toValue)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.Text (renderSvg)

import LSC.NetGraph
import LSC.Types



pixelsPerMicron :: Int
pixelsPerMicron = 60


type Scale = Double

zoomOut :: Scale -> Int -> Int
zoomOut scale = round . (/ scale) . fromIntegral



type Marker = Line Int

type Area = Component Layer Int



type Svg = S.Svg

type Arg = S.AttributeValue

type Args = (Arg, Arg)

m_, l_ :: Int -> Int -> S.Path
z_ :: S.Path
m_ = S.m
l_ = S.l
z_ = S.z



plotStdout :: Scale -> NetGraph -> IO ()
plotStdout scale = Lazy.putStr . renderSvg . plot scale



plot :: Scale -> NetGraph -> Svg
plot = svgDoc



svgDoc :: Scale -> NetGraph -> Svg
svgDoc scale top' = do

    let top = componentMap pixelated $ quadrantI top'

    S.docTypeSvg
      ! A.version "1.1"
      ! A.width  (toValue $ netGraphArea top ^. r)
      ! A.height (toValue $ netGraphArea top ^. t)
      $ do
        place scale `mapM_` view gates top
        route scale `mapM_` view nets top
        drawA ("black", "lightyellow") `mapM_` outerRim top

    where pixelated = fmap $ zoomOut scale . (* pixelsPerMicron)

          quadrantI ckt
              = flip componentMap ckt
              $ moveX (abs . min 0 $ netGraphArea ckt ^. l)
              . moveY (abs . min 0 $ netGraphArea ckt ^. b)



drawL :: Marker -> Svg
drawL (Line (x1, y1) (x2, y2)) = do

    S.path
      ! A.d (mkPath pen)
      ! A.stroke "black"
      ! A.strokeWidth "1"

    where pen = do
            m_ x1 y1
            l_ x2 y2
            z_



place :: Scale -> Gate -> Svg
place scale g = do

    let a = g ^. space

    let x = a ^. l
        y = a ^. b

    let f = zoomOut scale . (* pixelsPerMicron) . (* 20)

    drawA ("black", gateColor g) a

    S.text_
      ! A.x (toValue $ x + f 4)
      ! A.y (toValue $ y + f 12)
      ! A.fontSize (toValue $ f 15)
      ! A.fontFamily "monospace"
      ! A.transform (toValue $ "rotate(90 " <> decimal (x + f 4) <> "," <> decimal (y + f 12) <> ")")
      $ toSvg $ views number decimal g <> ": " <> views identifier (forShort 8) g



route :: Scale -> Net -> Svg
route _ _ = do
    pure ()



drawA :: Args -> Area -> Svg
drawA (border, background) a = do

    S.path
      ! A.d (mkPath pen)
      ! A.stroke border
      ! A.fill background
      ! A.strokeWidth "1"

    where pen = do
            m_ (a ^. l) (a ^. b)
            l_ (a ^. l) (a ^. t)
            l_ (a ^. r) (a ^. t)
            l_ (a ^. r) (a ^. b)
            z_



gateColor :: Gate -> Arg
gateColor g | g ^. feedthrough = "lightyellow"
gateColor g | g ^. fixed = "lightblue"
gateColor _ = "lightgrey"



forShort :: Int -> Text -> Builder
forShort n string
    | T.length string > n
    = fromText (T.take (n - 2) string) <> ".."
forShort _ string
    = fromText string

