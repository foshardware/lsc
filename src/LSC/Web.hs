
module LSC.Web where

import Control.Exception
import Data.Aeson
import Data.Text.Lazy
import Data.Text.Lazy.Encoding

import LSC.Types


routeWeb :: NetGraph -> LSC NetGraph
routeWeb = throw . AssertionFailed . unpack . decodeUtf8 . encode