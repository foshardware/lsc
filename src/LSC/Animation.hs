
module LSC.Animation where

import Control.Lens
import Data.Foldable

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort


type Frame = Picture

stdWindow :: Display
stdWindow = FullScreen -- InWindow "lsc" (640, 480) (10, 10)

stdColor :: Color
stdColor = black

runAnimation :: a -> (a -> Picture) -> (ViewPort -> Float -> a -> a) -> IO ()
runAnimation = simulate stdWindow stdColor 1

poly :: (Foldable f, Real r) => f (r, r) -> Frame
poly
  = color green
  . lineLoop
  . fmap (bimap realToFrac realToFrac)
  . toList

{-# SPECIALIZE poly :: [(Double, Double)] -> Frame #-}
