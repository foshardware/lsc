{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

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
import Data.Map hiding (null, toList, foldl', foldr, take)
import Data.Maybe
import Data.Serialize.Put
import Data.Text (unpack)
import Data.Text.Encoding
import Data.Vector (Vector)
import qualified Data.Vector as V
import Prelude hiding (lookup)

import LSC.Types



markEdges :: NetGraph -> [Line Integer]
markEdges top =
    [ Line (p^.l + po^.l, p^.b + po^.b) (q^.l + qo^.l, q^.b + qo^.b)
    | (_, net) <- top ^. nets . to assocs
    , (i, src) <- net ^. contacts . to assocs
    , any (\c -> c ^. dir == Just Out) src
    , (j, snk) <- net ^. contacts . to assocs
    , i /= j
    , p <- join $ toList $ top ^. gates ^? ix i . geometry . to (take 1)
    , q <- join $ toList $ top ^. gates ^? ix j . geometry . to (take 1)
    , po <- join $ take 1 src <&> view ports
    , qo <- join $ take 1 snk <&> view ports
    ]



inlineGeometry :: NetGraph -> NetGraph
inlineGeometry top = top &~ do

    gates .= gs
    nets .= rebuildEdges gs

    where

      gs = V.concat
        [ s ^. gates <&> project p
        | g <- toList $ top ^. gates
        , s <- toList $ lookup (g ^. identifier) (top ^. subcells)
        , p <- take 1 $ g ^. geometry
        ]

      project :: Component Layer Integer -> Gate -> Gate
      project p = geometry %~ fmap (\x -> x & l +~ p^.l & b +~ p^.b & t +~ p^.b & r +~ p^.l)



treeStructure :: NetGraph -> NetGraph
treeStructure top = top & subcells .~ foldr collect mempty (top ^. gates)
  where
    scope = fromList [ (x ^. identifier, x) | x <- flatten subcells top ]
    collect g a = maybe a (descend a) $ lookup (g ^. identifier) scope
    descend a n = insert (n ^. identifier) (n & subcells .~ foldr collect mempty (n ^. gates)) a



flatten :: Foldable f => Getter a (f a) -> a -> [a]
flatten descend netlist
  = netlist
  : join [ flatten descend model | model <- toList $ netlist ^. descend ]



netGraphStats :: NetGraph -> String
netGraphStats top = concat
  [ unwords
    [ top ^. identifier . to unpack <> ":"
    , unpack $ goedelIdentifier $ goedel top, "goedel,"
    , top ^. subcells . to length . to show, "subcells,"
    , top ^. supercell . pins . to length . to show, "pins,"
    , top ^. gates ^. to length . to show, "gates,"
    , top ^. nets ^. to length . to show, "nets"
    ]
  , unlines [ mempty | start ]
  , unlines
      [ netGraphStats n
      | n <- sortBy (compare `on` goedel) (top ^. subcells . to elems)
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

