
module LSC.Integer where

import Control.MILP.Types

import LSC.Types


routeInteger :: NetGraph -> LSC NetGraph
routeInteger top = do
  x <- liftLP $ do
    x <- general
    objective x
    x >=^ 1
    pure x
  result <- minimize
  debug [show $ result x]
  pure top
