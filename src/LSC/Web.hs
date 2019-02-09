
module LSC.Web where

import Data.Hashable

import LSC.Types


routeWeb :: NetGraph -> LSC NetGraph
routeWeb = fail . show . hash