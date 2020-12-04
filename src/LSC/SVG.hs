{-# LANGUAGE OverloadedStrings #-}

module LSC.SVG where

import Control.Lens
import Control.Monad
import Data.Foldable
import qualified Data.HashMap.Lazy as HashMap
import Data.String
import qualified Data.Text.Lazy    as Lazy
import qualified Data.Text.Lazy.IO as Lazy

import Data.Text.Lazy.Builder (toLazyText, fromText)
import Data.Text.Lazy.Builder.Int

import Text.Blaze.Svg11 ((!), mkPath, toSvg)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.Text (renderSvg)

import Prelude hiding (lookup)

import LSC.NetGraph
import LSC.Types



type Marker = Line Int

type Circuit = (Circuit2D Path, [Marker])

type Svg = S.Svg

type Arg = S.AttributeValue

type Args = (Arg, Arg)


scaleFactor' :: Int
scaleFactor' = 10


m_, l_:: Int -> Int -> S.Path
z_ :: S.Path
m_ = S.m
l_ = S.l
z_ = S.z


plotStdout :: NetGraph -> IO ()
plotStdout = Lazy.putStr . plot


plot :: NetGraph -> Lazy.Text
plot = renderSvg . svgDoc . scaleDown scaleFactor' . svgPaths


svgDoc :: Circuit -> Svg
svgDoc (Circuit2D ns edges, markers) = S.docTypeSvg
  ! A.version "1.1"
  ! A.width "10000000"
  ! A.height "10000000"
  $ do
    place `mapM_` ns
    route `mapM_` edges
    drawL `mapM_` markers


drawL :: Marker -> Svg
drawL (Line (x1, y1) (x2, y2)) = do
  S.path
    ! A.d (mkPath pen)
    ! A.stroke "blue"
    ! A.fill "blue"
    ! A.strokeWidth "1"
  where
    pen = do
      m_ x1 y1
      l_ x2 y2
      z_


place :: (Gate, Path) -> Svg
place (g, path@(p : _)) = do

  let (x, y) = (p ^. l, p ^. b)

  S.text_
    ! A.x (S.toValue $ x + 42)
    ! A.y (S.toValue $ y + 24)
    ! A.fontSize "24"
    ! A.fontFamily "monospace"
    ! A.transform (fromString $ "rotate(90 "++ show (x + 8) ++","++ show (y + 24)  ++")")
    $ renderText $ toLazyText $ decimal (view number g) <> ": " <> fromText (view identifier g)

  follow path

place _ = pure ()


route :: Arboresence Path -> Svg
route (_, pin, path) = do
  follow path
  follow pin


follow :: Path -> Svg
follow (p : xs) = do

  S.path
    ! A.d (mkPath pen)
    ! A.stroke (p ^. z . to stroke)
    ! A.fill (p ^. z . to fill)
    ! A.strokeWidth "1"

  follow xs

  where

    pen = do
      m_ (p ^. l) (p ^. b)
      l_ (p ^. l) (p ^. t)
      l_ (p ^. r) (p ^. t)
      l_ (p ^. r) (p ^. b)
      z_

follow _ = pure ()


stroke :: [Layer] -> Arg
stroke (Metal2 : Metal3 : _) = "black"
stroke (Metal1 : _) = "blue"
stroke (Metal2 : _) = "red"
stroke (Metal3 : _) = "green"
stroke _ = "black"

fill :: [Layer] -> Arg
fill (Metal2 : Metal3 : _) = "transparent"
fill (Metal1 : _) = "transparent"
fill (Metal2 : _) = "red"
fill (Metal3 : _) = "green"
fill _ = "transparent"


svgPaths :: NetGraph -> Circuit
svgPaths netlist = (Circuit2D gs ns, if null $ netlist ^. nets then markRouting netlist else [])

  where

    ns =
      [ (net, (outerPins net ++) . inducePins =<< net ^. contacts . to HashMap.toList, net ^. geometry)
      | net <- set geometry (netlist ^. supercell . mappend (vdd . geometry) (gnd . geometry)) mempty
      : toList (netlist ^. nets)
      ]

    gs =
      [ (gate, pure $ gate ^. space . to projectNorth)
      | gate <- toList $ netlist ^. gates
      ]

    outerPins :: Net -> Path
    outerPins net =
      [ port
      | pin <- toList $ netlist ^. supercell . pins
      , view identifier pin == view identifier net
      , port <- pin ^. geometry
      ]

    inducePins :: (Number, [Pin]) -> Path
    inducePins (i, ps) =
      [ q & l +~ p^.l & b +~ p^.b & r +~ p^.l & t +~ p^.b & integrate mempty
      | pin <- ps
      , p <- toList $ netlist ^. gates ^? ix i . space
      , q <- take 1 $ pin ^. geometry
      ]


scaleDown :: Int -> Circuit -> Circuit
scaleDown n (Circuit2D ns edges, markers) = (Circuit2D

  [ (gate, f (`div` n) path)
  | (gate, path) <- ns
  , let f = fmap . fmap
  ]

  [ (net, f (`div` n) ps, f (`div` n) path)
  | (net, ps, path) <- edges
  , let f = fmap . fmap
  ]

  , fmap (`div` n) <$> markers)


renderText :: Lazy.Text -> Svg
renderText string
    | Lazy.length string > 14
    = renderText $ ".." <> Lazy.drop (Lazy.length string - 12) string
renderText string
    = toSvg string

