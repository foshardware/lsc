-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module LSC.NetGraph where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.ST
import Data.Default
import Data.Foldable
import Data.Function
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.IntMap as M
import Data.List (sort, sortOn, sortBy, groupBy)
import Data.List.Split (wordsBy)
import Data.Maybe
import Data.Monoid
import Data.Text (unpack)
import Data.Matrix (Matrix, nrows, ncols, getElem, getMatrixAsVector)
import Data.Vector (Vector, (!), filter, backpermute)
import Data.Vector.Unboxed (unsafeFreeze)
import Data.Vector.Unboxed.Mutable (new, write)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import Prelude hiding (lookup, filter)

import LSC.Component
import LSC.Entropy
import LSC.Types



boundingBox :: (Foldable f, Integral n, Bounded n) => f (Component l n) -> Component l n
boundingBox = foldMap' implode 
{-# INLINABLE boundingBox #-}



coarseBoundingBox :: (Foldable f, Ord n, Bounded n) => f (Component l n) -> Component l n
coarseBoundingBox = foldMap' id
{-# INLINABLE coarseBoundingBox #-}



hpwl :: Net -> Int
hpwl = liftA2 (+) width height . foldMap' (implode . view space) . view members



hpwlDelta :: Foldable f => NetGraph -> f Gate -> Int
hpwlDelta top gs = sum
    [ width after - width before + height after - height before
    | net <- hyperedges top gs
    , let before = foldMap' (implode . view space)
                            (view members net)
    , let after  = foldMap' (\ g -> maybe (g ^. space . to implode) (implode . view space) $ find (eqNumber g) gs)
                            (view members net)
    ]
{-# INLINABLE hpwlDelta #-}



optimalRegion :: (Foldable f, Foldable g) => f (g Gate) -> Component l Int
optimalRegion f
    | all null f
    = error "optimalRegion: empty nodes"
optimalRegion f
    = rect x1 y1 x2 y2
    where
        boxes = foldr ((:) . foldMap' (implode . view space)) [] f
        [x1, x2] = medianElements $ sort $ foldMap (\ box -> [box ^. l, box ^. r]) boxes
        [y1, y2] = medianElements $ sort $ foldMap (\ box -> [box ^. b, box ^. t]) boxes
{-# INLINABLE optimalRegion #-}



hyperedges :: Foldable f => NetGraph -> f Gate -> [Net]
hyperedges top gs = map head . groupBy eqIdentifier . sortBy ordIdentifier $
    [ n
    | g <- toList gs
    , k <- toList $ view wires g
    , n <- toList $ top ^. nets ^? ix k
    ]
{-# INLINABLE hyperedges #-}



adjacentByPin :: NetGraph -> Gate -> HashMap Identifier (Vector Gate)
adjacentByPin top g
    = foldMap (filter (not . eqNumber g) . view members) . (view nets top ^?) . ix <$> view wires g



verticesByRow :: NetGraph -> Net -> [[(Gate, Pin)]]
verticesByRow top net
    = groupBy ((==) `on` view (_1 . space . b))
    $ sortOn (view (_1 . space . b))
    $ sortOn (view (_2 . geometry . to coarseBoundingBox . l))
      [ (view gates top ! i, p)
      | (i, ps) <- HashMap.toList $ net ^. contacts
      , i >= 0
      , p <- ps
      ]



markRouting :: NetGraph -> [Line Int]
markRouting top = join [ either horizontal vertical track | track <- top ^. supercell . tracks ]

  where

    horizontal p =
      [ Line (0, y) (x, y) :: Line Int
      | i <- [ 0 .. p ^. steps - 1 ]
      , x <- top ^. supercell . geometry <&> view r 
      , let y = i * p ^. trackSpace + p ^. offset
      ]

    vertical p =
      [ Line (x, 0) (x, y) :: Line Int
      | i <- [1 .. p ^. steps]
      , let x = i * p ^. trackSpace + p ^. offset
      , y <- top ^. supercell . geometry <&> view t 
      ]




markEdges :: NetGraph -> [Line Int]
markEdges top =
    [ Line (p^.l + po^.l, p^.b + po^.b) (q^.l + qo^.l, q^.b + qo^.b)
    | net <- toList $ top ^. nets
    , (i, src) <- net ^. contacts & HashMap.toList
    , any (\c -> c ^. dir == Just Out) src
    , (j, snk) <- net ^. contacts & HashMap.toList
    , i /= j
    , p <- toList $ top ^. gates ^? ix i . space
    , q <- toList $ top ^. gates ^? ix j . space
    , po <- join $ take 1 src <&> view geometry
    , qo <- join $ take 1 snk <&> view geometry
    ]



hpwlMatrix :: U.Vector (Int, Int) -> Net -> Int
hpwlMatrix _ n | elem (n ^. identifier) ["clk"] = 1
hpwlMatrix m n = width p + height p
  where
    p = boundingBox
      [ rect x y (succ x) (succ y)
      | i <- n ^. contacts & HashMap.keys
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



flattenGateMatrix :: Matrix Gate -> Vector Gate
flattenGateMatrix = filter (\ g -> g ^. number >= 0) . getMatrixAsVector



sumOfHpwlMatrix :: Matrix Gate -> Int
sumOfHpwlMatrix m = sum $ hpwlMatrix (coordsVector m) <$> generateEdges m



estimationsMatrix :: Matrix Gate -> LSC ()
estimationsMatrix m = do

  info
    [ show $ view number <$> m
    , unwords [show $ nrows m, "x", show $ ncols m]
    , unwords ["gate count:", show $ foldl' (\ a g -> if g ^. number < 0 then a else succ a :: Int) 0 m]
    , unwords [" net count:", show $ length $ generateEdges m]
    , unwords ["sum of hpwl:", show $ sumOfHpwlMatrix m]
    ]



estimations :: NetGraph -> LSC ()
estimations top = do
 
  let gs = top ^. gates
      ns = top ^. nets
 
  let box = coarseBoundingBox $ view space <$> gs

  let area = head (top ^. supercell . geometry)
      pivot = div (area ^. r + area ^. l) 2
  let (xs, ys) = V.partition ((<= pivot) . centerX) (view space <$> V.filter (views fixed not) gs)

  info
    [ unpack (view identifier top) ++ " layout area: " ++ show (width box, height box)
    , unpack (view identifier top) ++ " sum of hpwl: " ++ show (sum $ hpwl <$> ns)
    , unpack (view identifier top) ++ " gate count: "  ++ show (length gs)
    , unpack (view identifier top) ++ " row balance: "
      ++ show (sum $ width <$> xs) ++ " | " ++ show (sum $ width <$> ys)
    ]



significantHpwl :: NetGraph -> NetGraph -> Ordering
significantHpwl m n = div p x `compare` div q x
    where
        x = max 1 $ div p 10000 -- delta below 0.01% is equal
        p = sum $ hpwl <$> view nets m
        q = sum $ hpwl <$> view nets n



getSegments :: Vector Gate -> [Vector Gate]
getSegments
    = map V.fromList
    . join
    . map (wordsBy (view fixed))
    . map (sortOn (view space))
    . groupBy ((==) `on` view (space . b))
    . sortOn (view (space . b))
    . toList



getRows :: Vector Gate -> [Vector Gate]
getRows
    = map V.fromList
    . map (sortOn (view space))
    . groupBy ((==) `on` view (space . b))
    . sortOn (view (space . b))
    . toList



inlineGeometry :: NetGraph -> LSC NetGraph
inlineGeometry top = pure $ rebuildEdges $ top &~ do
 
    gates .= gs

    where

      gs = set number `imap` V.concat
        [ s ^. gates <&> project (view space g)
        | g <- toList $ top ^. gates
        , s <- toList $ top ^. subcells ^? views identifier ix g
        ]

      project :: Component Layer Int -> Gate -> Gate
      project p = space %~ \ x -> x & l +~ p^.l & b +~ p^.b & t +~ p^.b & r +~ p^.l



gateGeometry :: NetGraph -> LSC NetGraph
gateGeometry top = do

  tech <- technology

  assume ("invalid scale factor: " ++ views scaleFactor show tech)
    $ tech ^. scaleFactor > 0

  let fh = maximum $ tech ^. stdCells <&> snd . view dims
      fw = maximum $ top ^. supercell . rows <&> view granularity

  let expand g | g ^. feedthrough = g & space %~ \ x -> x & r .~ x^.l + fw & t .~ x^.b + fh
      expand g = g & space %~ maybe id drag (tech ^. stdCells ^? views identifier ix g . dims)

      drag (w, h) p = p & r .~ view l p + w & t .~ view b p + h

  pure $ top &~ do
      gates %= fmap expand



pinGeometry :: NetGraph -> LSC NetGraph
pinGeometry top = do

  tech <- technology

  assume ("invalid scale factor: " ++ views scaleFactor show tech)
    $ tech ^. scaleFactor > 0

  let align g p | g ^. feedthrough = p & geometry .~ pure (g ^. space)
      align g p = maybe p (absolute g) $ tech ^. stdCells ^? views identifier ix g . pins . views identifier ix p

      absolute g = over geometry
        $ fmap (l +~ g ^. space . l)
        . fmap (r +~ g ^. space . l)
        . fmap (b +~ g ^. space . b)
        . fmap (t +~ g ^. space . b)

  pure $ top &~ do
      nets %= fmap (over contacts . imap $ fmap . maybe id align . views gates (^?) top . ix)




perturbGates :: NetGraph -> LSC NetGraph
perturbGates top = do
    let getVector = randomPermutation $ top ^. gates . to length :: Gen s -> ST s Permutation
    v <- liftIO $ nonDeterministic Nothing getVector
    pure $ top &~ do
        gates .= set number `imap` views gates backpermute top v



assignCellsToRows :: NetGraph -> NetGraph
assignCellsToRows top
    | top ^. supercell . rows . to null
    = top
assignCellsToRows top
    = top & gates %~ fmap (over space $ relocateB =<< closest)
    where
      rs = top ^. supercell . rows
      closest s
        = fst
        $ minimumBy (compare `on` \ (y, _) -> abs (s ^. b - y))
        $ catMaybes [M.lookupGE (s ^. b) rs, M.lookupLE (s ^. b) rs]


assignCellsToColumns :: NetGraph -> NetGraph
assignCellsToColumns top
    = top & gates %~ fmap (over space $ relocateL =<< closest)
    where
      rs = top ^. supercell . rows <&> \ s -> (s ^. l, s ^. granularity)
      closest s
        | Just (off, step) <- rs ^? ix (s ^. b)
        , 0 <- mod (s ^. l - off) step
        = s ^. l
      closest s
        | Just (off, step) <- rs ^? ix (s ^. b)
        = s ^. l + step - mod (s ^. l - off) step
      closest s
        = s ^. l



removeFeedthroughs :: NetGraph -> NetGraph
removeFeedthroughs = gates %~ imap (set number) . filter (views feedthrough not)



rebuildEdges :: NetGraph -> NetGraph
rebuildEdges = liftA2 (set nets) (generateEdges . view gates) id



generateEdges :: Foldable f => f Gate -> HashMap Identifier Net
generateEdges gs = HashMap.fromListWith (<>)
    [ (i, Net i mempty (pure gate) (HashMap.singleton (gate ^. number) [pin]))
    | gate <- toList gs
    , (contact, i) <- HashMap.toList $ gate ^. wires
    , let pin = def & identifier .~ contact
    ]
{-# INLINABLE generateEdges #-}


componentMap :: (Component Layer Int -> Component Layer Int) -> NetGraph -> NetGraph
componentMap f top = top &~ do
    supercell %= (over pins . fmap . over geometry . fmap) f
    gates %= (fmap . over space) f
    nets %= (fmap . over contacts . fmap . fmap . over geometry . fmap) f



netGraphArea :: NetGraph -> Component l Int
netGraphArea top
    = castLayer
    $ coarseBoundingBox (view space <$> view gates top) <> coarseBoundingBox (outerRim top)



outerRim :: NetGraph -> HashMap Identifier (Component Layer Int)
outerRim = fmap (coarseBoundingBox . view geometry) . view (supercell . pins)



gateWidth, gateHeight :: Gate -> Int
gateWidth  = width  . view space
gateHeight = height . view space



gateOverlap :: Gate -> Gate -> Bool
gateOverlap x y = areaOverlap (x ^. space) (y ^. space)



invert :: Pin -> Pin
invert pin | pin ^. dir == pure  In = pin & dir .~ pure Out
invert pin | pin ^. dir == pure Out = pin & dir .~ pure  In
invert pin = pin



treeStructure :: NetGraph -> NetGraph
treeStructure top = top & subcells .~ foldr collect mempty (top ^. gates)
  where
    scope = HashMap.fromList [ (x ^. identifier, x) | x <- flatten subcells top ]
    collect g a = maybe a (descend a) $ scope ^? views identifier ix g
    descend a n = HashMap.insert (n ^. identifier) (n & subcells .~ foldr collect mempty (n ^. gates)) a



flatten :: Foldable f => Getter a (f a) -> a -> [a]
flatten descend netlist
  = netlist
  : join [ flatten descend model | model <- toList $ netlist ^. descend ]



leaves :: NetGraph -> [NetGraph]
leaves top | top ^. subcells . to null = pure top
leaves top = join $ top ^. subcells & toList <&> leaves

