
module LSC.NetGraph where

import Control.Lens
import Control.Monad
import Data.Bits
import Data.ByteString.Base16
import Data.Default
import Data.Foldable
import Data.Function
import Data.Hashable
import qualified Data.IntSet as S
import Data.List (sortBy)
import Data.Map hiding (null, toList, foldl')
import Data.Serialize.Put
import Data.Text (unpack)
import Data.Text.Encoding
import Data.Vector (Vector)
import Prelude hiding (lookup)

import LSC.Types


netGraphStats :: NetGraph -> String
netGraphStats top = concat
  [ unwords
    [ top ^. identifier . to unpack <> ":"
    , unpack $ goedelIdentifier $ netGraphGoedel top, "goedel,"
    , top ^. subcells . to length . to show, "subcells,"
    , top ^. supercell . pins . to length . to show, "pins,"
    , top ^. gates ^. to length . to show, "gates,"
    , top ^. nets ^. to length . to show, "nets"
    ]
  , unlines [ mempty | start ]
  , unlines
      [ netGraphStats n
      | n <- sortBy (compare `on` netGraphGoedel) (top ^. subcells . to elems)
      ]
  , unlines [ "" | start ]
  , unlines [ top ^. subcells . to length . to show
             <> " subcells." | start ]
  , unlines [ show (S.size $ S.fromList $ top ^. subcells . to elems <&> goedel)
             <> " distinct subcells." | start ]
  ] where
      goedel = netGraphGoedel
      start = not $ top ^. subcells . to null


goedelIdentifier :: Int -> Identifier
goedelIdentifier = decodeUtf8 . encode . runPut . putInt32be . fromIntegral


netGraphGoedel :: NetGraph -> Int
netGraphGoedel top = foldl' xor outerPins $ gateGoedel wireSequence <$> top ^. gates
  where
    outerPins = hash $ elems $ intersection wireSequence $ top ^. supercell . pins
    wireSequence = fromAscList $ zip (top ^. nets . to keys) [0..]

gateGoedel :: Map Identifier Int -> Gate -> Int
gateGoedel ns g = hash [ (lookup w ns, view identifier g) | w <- g ^. wires . to elems ]



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

