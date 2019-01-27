{-# LANGUAGE OverloadedStrings #-}

module LSC.SVG where

import Control.Lens
import Data.Foldable
import Data.String
import Data.Map (assocs)
import Data.Text hiding (take)
import Data.Vector (indexM)
import qualified Data.Text as Text
import qualified Data.Text.Lazy    as Lazy
import qualified Data.Text.Lazy.IO as Lazy

import Text.Blaze.Svg11 ((!), mkPath)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.Text (renderSvg)

import LSC.Types


type Circuit = Circuit2D Path

type Svg = S.Svg

type Arg = S.AttributeValue

type Args = (Arg, Arg)

m_, l_:: Integer -> Integer -> S.Path
z_ :: S.Path
m_ = S.m
l_ = S.l
z_ = S.z


plotStdout :: NetGraph -> IO ()
plotStdout = Lazy.putStr . plot


plot :: NetGraph -> Lazy.Text
plot = renderSvg . svgDoc . scaleDown 100 . svgPaths


svgDoc :: Circuit -> Svg
svgDoc (Circuit2D nodes edges) = S.docTypeSvg
  ! A.version "1.1"
  ! A.width "100000"
  ! A.height "100000"
  $ do
    place `mapM_` nodes
    route `mapM_` edges


place :: (Gate, Path) -> Svg
place (g, path@(p : _)) = do

  let (x, y) = (p ^. l, p ^. b)

  S.text_
    ! A.x (S.toValue $ x + 42)
    ! A.y (S.toValue $ y + 24)
    ! A.fontSize "24"
    ! A.fontFamily "monospace"
    ! A.transform (fromString $ "rotate(90 "++ show (x + 8) ++","++ show (y + 24)  ++")")
    $ renderText $ g ^. identifier

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
stroke (Metal3 : Metal2 : _) = "black"
stroke (Metal1 : _) = "blue"
stroke (Metal2 : _) = "red"
stroke (Metal3 : _) = "green"
stroke _ = "black"

fill :: [Layer] -> Arg
fill (Metal3 : Metal2 : _) = "transparent"
fill (Metal1 : _) = "blue"
fill (Metal2 : _) = "red"
fill (Metal3 : _) = "green"
fill _ = "transparent"


svgPaths :: NetGraph -> Circuit
svgPaths netlist = Circuit2D

  [ (gate, gate ^. geometry)
  | gate <- toList $ netlist ^. gates
  ]

  [ (net, outerPins net ++ (inducePins =<< assocs (net ^. contacts)), net ^. geometry)
  | net <- set geometry (netlist ^. supercell . mappend (vdd . ports) (gnd . ports)) mempty
  : toList (netlist ^. nets)
  ]

  where

    outerPins :: Net -> Path
    outerPins net =
      [ port
      | pin <- toList $ netlist ^. supercell . pins
      , view identifier pin == view identifier net
      , port <- pin ^. ports
      ]

    inducePins :: (Number, [Pin]) -> Path
    inducePins (i, ps) =
      [ Rect (q ^. l + p ^. l) (q ^. b + p ^. b) (q ^. r + p ^. l) (q ^. t + p ^. b)
      | pin <- ps
      , p <- take 1 . view geometry =<< indexM (netlist ^. gates) i
      , q <- take 1 $ pin ^. ports
      ]


scaleDown :: Integer -> Circuit -> Circuit
scaleDown n (Circuit2D nodes edges) = Circuit2D

  [ (gate, f (`div` n) path)
  | (gate, path) <- nodes
  , let f = fmap . fmap
  ]

  [ (net, f (`div` n) ps, f (`div` n) path)
  | (net, ps, path) <- edges
  , let f = fmap . fmap
  ]


renderText :: Text -> Svg
renderText = fromString . Text.unpack

