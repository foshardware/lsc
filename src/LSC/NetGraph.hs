
module LSC.NetGraph where

import Control.Lens
import Control.Monad
import Data.Default
import Data.Foldable
import Data.Map (Map, assocs, elems, singleton, fromListWith)
import Data.Text (unpack)
import Data.Vector (Vector)

import LSC.Types


netGraphStats :: NetGraph -> String
netGraphStats top = concat
  [ unwords
    [ top ^. identifier . to unpack <> ":"
    , top ^. subcells . to length . to show, "subcells,"
    , top ^. supercell . pins . to length . to show, "pins,"
    , top ^. gates ^. to length . to show, "gates,"
    , top ^. nets ^. to length . to show, "nets"
    ]
  , unlines [ "" | not $ top ^. subcells . to null ]
  , unlines [ netGraphStats n | n <- top ^. subcells . to elems ]
  ]


rebuildEdges :: Vector Gate -> Map Identifier Net
rebuildEdges nodes = fromListWith (<>)
    [ (net, Net net mempty (singleton (gate ^. number) [pin]))
    | gate <- toList nodes
    , (contact, net) <- gate ^. wires & assocs
    , let pin = def & identifier .~ contact
    ]


leaves :: NetGraph -> [NetGraph]
leaves top | top ^. subcells . to null = pure top
leaves top = join $ top ^. subcells . to elems <&> leaves

