
module LSC.Web where

import Control.Exception
import Data.Hashable

import LSC.Types


routeWeb :: NetGraph -> LSC NetGraph
routeWeb = throw . AssertionFailed . show . hash