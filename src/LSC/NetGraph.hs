{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.IntSet as S
import Data.List (sortBy)
import Data.Map hiding (null, toList, foldl', foldr, take, filter)
import Data.Maybe
import Data.Serialize.Put
import Data.Text (unpack)
import Data.Text.Encoding
import Data.Matrix (Matrix, nrows, ncols, getElem, getMatrixAsVector)
import Data.Vector (Vector, imap, filter)
import Data.Vector.Unboxed (unsafeFreeze)
import Data.Vector.Unboxed.Mutable (new, write)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import Prelude hiding (lookup, filter)

import LSC.Types


boundingBox :: (Foldable f, Functor f, Integral n) => f (Component l n) -> Component l n
boundingBox cs | null cs = error "boundingBox: no components"
boundingBox cs = Rect (minimum x) (minimum y) (maximum x) (maximum y)
    where
        x = fmap (\ c -> div (view r c + view l c) 2) cs
        y = fmap (\ c -> div (view t c + view b c) 2) cs
{-# SPECIALIZE boundingBox ::        [Component l Int] -> Component l Int #-}
{-# SPECIALIZE boundingBox :: Vector (Component l Int) -> Component l Int #-}
{-# SPECIALIZE boundingBox ::        [Component l Integer] -> Component l Integer #-}
{-# SPECIALIZE boundingBox :: Vector (Component l Integer) -> Component l Integer #-}



coarseBoundingBox :: (Foldable f, Functor f, Ord n) => f (Component l n) -> Component l n
coarseBoundingBox cs | null cs = error "coarseBoundingBox: no components"
coarseBoundingBox cs = Rect
    (minimum $ view l <$> cs)
    (minimum $ view b <$> cs)
    (maximum $ view r <$> cs)
    (maximum $ view t <$> cs)
{-# SPECIALIZE coarseBoundingBox ::        [Component l Int] -> Component l Int #-}
{-# SPECIALIZE coarseBoundingBox :: Vector (Component l Int) -> Component l Int #-}
{-# SPECIALIZE coarseBoundingBox ::        [Component l Integer] -> Component l Integer #-}
{-# SPECIALIZE coarseBoundingBox :: Vector (Component l Integer) -> Component l Integer #-}



hpwlMatrix :: U.Vector (Int, Int) -> Net -> Int
hpwlMatrix _ n | elem (n ^. identifier) ["clk"] = 1
hpwlMatrix m n = width p + height p
  where
    p = boundingBox
      [ Rect x y (succ x) (succ y)
      | (i, _) <- n ^. contacts . to assocs
      , (x, y) <- toList $ m ^? ix i
      ]



coordsVector :: Matrix Gate -> U.Vector (Int, Int)
coordsVector m = runST $ do
    u <- new $ succ $ maximum $ view number <$> m
    sequence_
        [ write u g (i, j)
        | i <- [1 .. nrows m]
        , j <- [1 .. ncols m]
        , let g = getElem i j m ^. number
        , g >= 0
        ]
    unsafeFreeze u



hpwl :: Vector Gate -> Net -> Integer
hpwl  _ n | elem (n ^. identifier) ["clk", "CLK"] || elem (n ^. identifier) power = 0
hpwl gs n = width p + height p
  where
    p = boundingBox $ catMaybes
      [ gs ^? ix i . space 
      | (i, _) <- n ^. contacts . to assocs
      ]


hpwlDelta :: Foldable f => NetGraph -> f Gate -> Integer
hpwlDelta top gs
  = sum (width  <$> qs) - sum (width  <$> ps)
  + sum (height <$> qs) - sum (height <$> ps)
  where
    ns = HashSet.fromList $ catMaybes
      [ top ^. nets ^? ix k
      | ks <- views wires elems <$> toList gs
      , k <- ks
      , k /= "clk", k /= "CLK", not $ k `elem` power
      ]
    ps =
      [ boundingBox $ fmap (view space) $ catMaybes $ flip preview (top ^. gates) . ix <$> is
      | n <- toList ns
      , let is = views contacts keys n
      ]
    qs =
      [ boundingBox $ fmap (view space) $ mappend hs $ catMaybes $ flip preview (top ^. gates) . ix <$> is
      | n <- toList ns
      , let is = [ i | i <- views contacts keys n, all ((/= i) . view number) gs ]
      , let hs = join [ [ g | g <- toList gs, view number g == h ] | h <- views contacts keys n ]
      ]
{-# SPECIALIZE hpwlDelta :: NetGraph -> Vector Gate -> Integer #-}
{-# SPECIALIZE hpwlDelta :: NetGraph -> [Gate] -> Integer #-}


markRouting :: NetGraph -> [Line Integer]
markRouting top = join [ either horizontal vertical track | track <- top ^. supercell . tracks ]

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
    | net <- toList $ top ^. nets
    , (i, src) <- net ^. contacts . to assocs
    , any (\c -> c ^. dir == Just Out) src
    , (j, snk) <- net ^. contacts . to assocs
    , i /= j
    , p <- toList $ top ^. gates ^? ix i . space
    , q <- toList $ top ^. gates ^? ix j . space
    , po <- join $ take 1 src <&> view geometry
    , qo <- join $ take 1 snk <&> view geometry
    ]



flattenGateMatrix :: Matrix Gate -> Vector Gate
flattenGateMatrix = filter (\ g -> g ^. number >= 0) . getMatrixAsVector



sumOfHpwlMatrix :: Matrix Gate -> Int
sumOfHpwlMatrix m = sum $ hpwlMatrix (coordsVector m) <$> rebuildEdges m



estimationsMatrix :: Matrix Gate -> LSC ()
estimationsMatrix m = do

  debug
    [ show $ view number <$> m
    , unwords [show $ nrows m, "x", show $ ncols m]
    , unwords ["gate count:", show $ foldl' (\ a g -> if g ^. number < 0 then a else succ a :: Int) 0 m]
    , unwords [" net count:", show $ length $ rebuildEdges m]
    , unwords ["sum of hpwl:", show $ sumOfHpwlMatrix m]
    ]



estimations :: NetGraph -> LSC ()
estimations top = do
 
  let gs = top ^. gates
      ns = top ^. nets
 
  let box = boundingBox $ view space <$> gs

  debug
    [ unpack (view identifier top) ++ " layout area: " ++ show (width box, height box)
    , unpack (view identifier top) ++ " sum of hpwl: " ++ show (sum $ hpwl gs <$> ns)
    , unpack (view identifier top) ++ " gate count: "  ++ show (length gs)
    ]



significantHpwl :: NetGraph -> NetGraph -> Integer
significantHpwl m n = sw m - sw n - div (sw n) 10000
    where sw q = sum $ hpwl (view gates q) <$> view nets q



inlineGeometry :: NetGraph -> LSC NetGraph
inlineGeometry top = pure $ top &~ do
 
    gates .= gs
    nets .= ns

    where

      ns = rebuildEdges gs

      gs = set number `imap` V.concat
        [ s ^. gates <&> project (view space g)
        | g <- toList $ top ^. gates
        , s <- toList $ lookup (g ^. identifier) (top ^. subcells)
        ]

      project :: Component Layer Integer -> Gate -> Gate
      project p = space %~ \ x -> x & l +~ p^.l & b +~ p^.b & t +~ p^.b & r +~ p^.l



gateGeometry :: NetGraph -> LSC NetGraph
gateGeometry netlist = do

  cells <- view stdCells <$> technology

  let expand g = g & space %~ maybe id drag (cells ^? ix (g ^. identifier) . dims)
      drag (w, h) p = p &~ do
          r .= view l p + w
          t .= view b p + h

  pure $ netlist &~ do
      gates %= fmap expand


assignCellsToRows :: NetGraph -> LSC NetGraph
assignCellsToRows top = do

    let rs = S.fromList $ toList $ top ^. supercell . rows <&> views b fromIntegral

    let closest x = minimumBy (compare `on` \ y -> abs (x - y)) $ catMaybes [S.lookupGE x rs, S.lookupLE x rs]

    pure $ top &~ do
        gates %= fmap (space %~ (relocateB =<< fromIntegral . closest . fromIntegral . view b))



power :: HashSet Identifier
power = HashSet.fromList ["gnd", "vdd", "vcc", "GND", "VDD", "VCC"]


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

    createNets tech = HashMap.fromListWith mappend
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
    wireSequence = fromAscList $ zip (top ^. nets . to HashMap.keys) [0..]

gateGoedel :: Map Identifier Int -> Gate -> Int
gateGoedel ns g = hash [ (lookup w ns, view identifier g) | w <- g ^. wires . to elems ]



rebuildEdges :: Foldable f => f Gate -> HashMap Identifier Net
rebuildEdges nodes = HashMap.fromListWith (<>)
    [ (net, Net net mempty (singleton (gate ^. number) [pin]))
    | gate <- toList nodes
    , (contact, net) <- gate ^. wires & assocs
    , let pin = def & identifier .~ contact
    ]
{-# SPECIALIZE rebuildEdges :: Matrix Gate -> HashMap Identifier Net #-}
{-# SPECIALIZE rebuildEdges :: Vector Gate -> HashMap Identifier Net #-}


leaves :: NetGraph -> [NetGraph]
leaves top | top ^. subcells . to null = pure top
leaves top = join $ top ^. subcells . to elems <&> leaves

