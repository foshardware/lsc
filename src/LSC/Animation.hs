
module LSC.Animation where

import Graphics.Gloss


type Frame = Picture

stdWindow = InWindow "lsc" (640, 480) (10, 10)
stdColor = white

runAnimation = simulate stdWindow stdColor 1

rectangle = rectangleWire

poly = lineLoop
