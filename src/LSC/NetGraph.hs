
module LSC.NetGraph where

import Control.Lens
import Data.Map (assocs)
import Data.Text (unpack)

import LSC.Types


netGraphStats :: NetGraph -> String
netGraphStats top = unlines $
  [ unwords
    [ top ^. identifier . to unpack <> ":"
    , top ^. subcells . to length . to show, "subcells,"
    , top ^. supercell . pins . to length . to show, "pins,"
    , top ^. gates ^. to length . to show, "gates,"
    , top ^. nets ^. to length . to show, "nets"
    ]
  ] <>
  [ unwords
    [ unpack i <> ":"
    , n ^. supercell . pins . to length . to show, "pins,"
    , n ^. gates ^. to length . to show, "gates,"
    , n ^. nets ^. to length . to show, "nets"
    ]
  | (i, n) <- top ^. subcells . to assocs
  ] <>
  []
