
module LSC.Animation where

import Graphics.Gloss


type Frame = Picture

stdWindow = FullScreen -- InWindow "lsc" (640, 480) (10, 10)
stdColor = black

runAnimation = simulate stdWindow stdColor 1

poly = color green . lineLoop
