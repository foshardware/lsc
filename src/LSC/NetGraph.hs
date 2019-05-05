
module LSC.NetGraph where

import Control.Lens
import Data.Default
import Data.Foldable
import Data.Map (Map, assocs, singleton, fromListWith)
import Data.Text (unpack)
import Data.Vector (Vector)

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
  [ netGraphStats n | (_, n) <- top ^. subcells . to assocs ]


rebuildEdges :: Vector Gate -> Map Identifier Net
rebuildEdges nodes = fromListWith (<>)
    [ (net, Net net mempty (singleton (gate ^. number) [pin]))
    | gate <- toList nodes
    , (contact, net) <- gate ^. wires & assocs
    , let pin = def & identifier .~ contact
    ]

