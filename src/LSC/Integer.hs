
module LSC.Integer where

import Control.MILP.Types

import LSC.Types


routeInteger :: NetGraph -> LSC NetGraph
routeInteger top = do
  x <- liftInteger $ do
    x <- general
    objective x
    x >=^ 1
    pure x
  result <- minimizeInteger
  debug [show $ result x]
  pure top
