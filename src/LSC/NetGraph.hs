
module LSC.NetGraph where

import Control.Lens
import Data.Map (assocs)
import Data.Text (unpack)

import LSC.Types


netGraphStats :: NetGraph -> String
netGraphStats top = unlines $
  [ unwords
    [ top ^. identifier . to unpack <> ":"
    , top ^. supercell . pins . to length . to show
    ]
  , "children: " <> top ^. subcells . to length . to show
  ] ++
  [ unpack i <> ": " <> show (length x)
  | (i, n) <- top ^. subcells . to assocs
  , let x = n ^. supercell . pins
  ] ++
  []
