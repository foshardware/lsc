{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module LSC.NetGraph where

import Control.Applicative
import Control.Lens hiding (imap)
import Control.Monad
import Control.Monad.ST
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
import Data.Matrix (Matrix, nrows, ncols, getElem, getRow)
import Data.Vector (Vector, imap)
import Data.Vector.Unboxed (unsafeFreeze)
import Data.Vector.Unboxed.Mutable (new, write)
import qualified Data.Vector as V
import Prelude hiding (lookup)

import LSC.Types



boundingBox :: Ord n => [Component l n] -> Component l n
boundingBox xs = Rect
    (minimum $ view l <$> xs)
    (minimum $ view b <$> xs)
    (maximum $ view r <$> xs)
    (maximum $ view t <$> xs)



hpwlMatrix :: Matrix Gate -> Net -> Int
hpwlMatrix m n = width p + height p

  where

    p = boundingBox nodes

    coords = runST $ do
        v <- new $ nrows m * ncols m
        sequence_
            [ write v g (i, j)
            | i <- [1 .. nrows m]
            , j <- [1 .. ncols m]
            , let g = getElem i j m ^. number
            , g >= 0
            ]

        unsafeFreeze v

    nodes =
      [ Rect x y (succ x) (succ y)
      | (i, _) <- n ^. contacts . to assocs
      , (x, y) <- toList $ coords ^? ix i
      ]




hpwl :: Vector Gate -> Net -> Integer
hpwl gs n = width p + height p
  where
    p = boundingBox nodes
    nodes = catMaybes
      [ join $ gs ^? ix i . geometry . to listToMaybe
      | (i, _) <- n ^. contacts . to assocs
      ]



markRouting :: NetGraph -> [Line Integer]
markRouting top = join [ either horizontal vertical track | track <- top ^. supercell . routing ]

  where

    horizontal p =
      [ Line (0, y) (x, y) :: Line Integer
      | i <- [ 0 .. p ^. steps - 1 ]
      , x <- top ^. supercell . geometry <&> view r 
      , let y = fromIntegral i * p ^. space + p ^. offset
      ]

    vertical p =
      [ Line (x, 0) (x, y) :: Line Integer
      | i <- [1 .. p ^. steps]
      , let x = fromIntegral i * p ^. space + p ^. offset
      , y <- top ^. supercell . geometry <&> view t 
      ]




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



sumOfHpwlMatrix :: Matrix Gate -> Int
sumOfHpwlMatrix m = do

  let v = mconcat [ getRow i m | i <- [1 .. nrows m] ]
      e = rebuildEdges v

  sum $ hpwlMatrix m <$> e



estimationsMatrix :: Matrix Gate -> LSC ()
estimationsMatrix m = do


  debug
    [ show $ view number <$> m
    , "sum of hpwl: " ++ show (sumOfHpwlMatrix m)
    ]



estimations :: NetGraph -> LSC ()
estimations top = do
 
  let gs = top ^. gates
      ns = top ^. nets
 
  let box = boundingBox [ p | g <- toList gs, p <- take 1 $ g ^. geometry ]

  debug
    [ unpack (view identifier top) ++ " layout area: " ++ show (width box, height box)
    , unpack (view identifier top) ++ " sum of hpwl: " ++ show (sum $ hpwl gs <$> ns)
    ]



inlineGeometry :: NetGraph -> LSC NetGraph
inlineGeometry top = pure $ top &~ do
 
    gates .= gs
    nets .= ns

    where

      ns = rebuildEdges gs

      gs = set number `imap` V.concat
        [ s ^. gates <&> project p
        | g <- toList $ top ^. gates
        , s <- toList $ lookup (g ^. identifier) (top ^. subcells)
        , p <- take 1 $ g ^. geometry
        ]

      project :: Component Layer Integer -> Gate -> Gate
      project p = geometry %~ fmap (\x -> x & l +~ p^.l & b +~ p^.b & t +~ p^.b & r +~ p^.l)



gateGeometry :: NetGraph -> LSC NetGraph
gateGeometry netlist = do

  cells <- view stdCells <$> technology

  let expand g = g & geometry %~ maybe id (fmap . drag) (cells ^? ix (g ^. identifier) . dims)
      drag (w, h) p = p & t +~ h & r +~ w

  pure $ netlist &~ do
      gates %= fmap expand



contactGeometry :: NetGraph -> LSC NetGraph
contactGeometry netlist = do

  tech <- technology

  pure $ netlist &~ do
      gates %= fmap (vddGnd tech)
      nets  .= createNets tech

  where

    vddGnd tech g
      | Just sc <- view identifier g `lookup` view stdCells tech
      = g & vdd .~ view vdd sc & gnd .~ view gnd sc  
    vddGnd _ g
      | Just sc <- view identifier g `lookup` view subcells netlist <&> view supercell
      = g & vdd .~ view vdd sc & gnd .~ view gnd sc
    vddGnd _ g = g

    createNets tech = fromListWith mappend
      [ (net, Net net mempty (singleton (gate ^. number) [pin]))
      | gate <- toList $ netlist ^. gates
      , (contact, net) <- assocs $ gate ^. wires
      , let key = gate ^. identifier
      , let scope = lookup contact
      , pin <- toList
            $ join (tech ^. stdCells ^? ix key . pins . to scope)
          <|> join (netlist ^. subcells ^? ix key . supercell . pins . to scope)
      ]



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

