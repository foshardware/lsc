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
import qualified Data.IntSet as IntSet
import Data.List (sort, sortOn)
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

import LSC.Cartesian
import LSC.Component
import LSC.Entropy
import LSC.Types
import LSC.Polygon



boundingBox
  :: (Foldable f, Integral x, Integral y, Bounded x, Bounded y)
  => f (Component l x y) -> Component l x y
boundingBox = foldMap' implode 
{-# INLINABLE boundingBox #-}



coarseBoundingBox
  :: (Foldable f, Ord x, Ord y, Bounded x, Bounded y)
  => f (Component l x y) -> Component l x y
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
  , let after  = foldMap' (\ g -> maybe (g ^. space . to implode) (implode . view space)
                                $ find (\ h -> g ^. number == h ^. number) gs)
                          (view members net)
  ]
{-# INLINABLE hpwlDelta #-}



optimalRegion :: (Foldable f, Foldable g) => f (g Gate) -> Component' l Int
optimalRegion f
  | all null f
  = error "optimalRegion: empty nodes"
optimalRegion f
  = rect x1 y1 x2 y2
  where
    boxes = foldr ((:) . foldMap' (implode . view space)) [] f
    [x1, x2] = medianElements $ sort $ foldMap abscissae boxes
    [y1, y2] = medianElements $ sort $ foldMap ordinates boxes
{-# INLINABLE optimalRegion #-}



hyperedges :: Foldable f => NetGraph -> f Gate -> [Net]
hyperedges top gs
  = map getDistinctIdentifier
  . unstableUnique
  $ [ DistinctIdentifier n
    | g <- toList gs
    , k <- toList $ g ^. wires
    , n <- toList $ top ^. nets ^? ix k
    ]
{-# INLINABLE hyperedges #-}



adjacentByPin :: NetGraph -> Gate -> HashMap Identifier (Vector Gate)
adjacentByPin top g
  = foldMap (filter (\ h -> g ^. number /= h ^. number) . view members)
  . flip preview (view nets top) . ix
    <$> view wires g



verticesByRow :: NetGraph -> Net -> [[(Gate, Pin)]]
verticesByRow top
  = groupOn (view (_1 . space . b))
  . sortOn (view (_1 . space . b))
  . verticesOf top


verticesByColumn :: NetGraph -> Net -> [[(Gate, Pin)]]
verticesByColumn top net =
  [ [ (g, p & geometry .~ [simplePolygon c])
    | c <- foldMap (columns (row ^. l) (row ^. granularity)) (p ^. geometry)
    ]
  | (g, p) <- verticesOf top net
  , row <- toList $ top ^. supercell . rows ^? ix (g ^. space . b)
  ]


verticesOf :: NetGraph -> Net -> [(Gate, Pin)]
verticesOf top net =
  [ (view gates top ! i, p)
  | (i, ps) <- HashMap.toList $ net ^. contacts
  , i >= 0
  , p <- ps
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
sumOfHpwlMatrix m = sum $ hpwlMatrix (coordsVector m) <$> generateHyperedges m



estimationsMatrix :: Matrix Gate -> LSC ()
estimationsMatrix m = do

  info
    [ show $ view number <$> m
    , unwords [show $ nrows m, "x", show $ ncols m]
    , unwords ["gate count:", show $ foldl' (\ a g -> if g ^. number < 0 then a else succ a :: Int) 0 m]
    , unwords [" net count:", show $ length $ generateHyperedges m]
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

  let segs = sum $ length . view netSegments <$> ns

  let k = views identifier unpack top

  info $
    [ k ++ " layout area: " ++ show (width box, height box)
    , k ++ " sum of hpwl: " ++ show (sum $ hpwl <$> ns)
    , k ++ " gate count: "  ++ show (length gs)
    ]
    ++ [ k ++ " net segments: " ++ show segs | segs > 0 ]
    ++ [ k ++ " row balance: " ++ show (sum $ width <$> xs) ++ " | " ++ show (sum $ width <$> ys) ]



significantHpwl :: NetGraph -> NetGraph -> Ordering
significantHpwl m n = div p x `compare` div q x
    where
        x = max 1 $ div p 10000 -- delta below 0.01% is equal
        p = sum $ hpwl <$> view nets m
        q = sum $ hpwl <$> view nets n



getSegments :: Vector Gate -> [Vector Gate]
getSegments
  = map V.fromList
  . foldMap (wordsBy (view fixed))
  . map (sortOn (view space))
  . groupOn (view (space . b))
  . sortOn (view (space . b))
  . toList



getRows :: Vector Gate -> [Vector Gate]
getRows
  = map V.fromList
  . map (sortOn (view space))
  . groupOn (view (space . b))
  . sortOn (view (space . b))
  . toList



getRow :: NetGraph -> Gate -> Maybe Row
getRow top g = top ^. supercell . rows ^? ix (g ^. space . b)




inlineGeometry :: NetGraph -> NetGraph
inlineGeometry top
  = rebuildHyperedges
  $ top &~ do
    gates .= set number `imap` V.concat
      [ s ^. gates <&> over space (inline (g ^. space))
      | g <- toList $ top ^. gates
      , s <- toList $ top ^. subcells ^? views identifier ix g
      ]



gateGeometry :: NetGraph -> LSC NetGraph
gateGeometry top = do
    tech <- technology
    assume ("invalid scale factor: " ++ views scaleFactor show tech)
      $ tech ^. scaleFactor > 0
    pure $ rebuildGates (tech ^. stdCells) top


rebuildGates :: HashMap Identifier Cell -> NetGraph -> NetGraph
rebuildGates cells top = top &~ do

    gates %= fmap expand

    where

    fh = maximum $ cells <&> snd . view dims
    fw = maximum $ top ^. supercell . rows <&> view granularity

    expand g | g ^. feedthrough = g & space %~ \ x -> x & r .~ x^.l + fw & t .~ x^.b + fh
    expand g = g & space %~ maybe id drag (cells ^? views identifier ix g . dims)

    drag (w, h) p = p & r .~ view l p + w & t .~ view b p + h



pinGeometry :: NetGraph -> LSC NetGraph
pinGeometry top = do
    tech <- technology
    assume ("invalid scale factor: " ++ views scaleFactor show tech)
      $ tech ^. scaleFactor > 0
    pure $ rebuildPins (tech ^. stdCells) top


rebuildPins :: HashMap Identifier Cell -> NetGraph -> NetGraph
rebuildPins cells top = top &~ do

    nets %= fmap (over contacts . imap $ fmap . maybe id align . views gates (^?) top . ix)

  where

    align g p
      | g ^. feedthrough
      = p & geometry .~ pure (g ^. space . to simplePolygon)
    align g p
      = maybe p (absolute g) $ cells ^? views identifier ix g . pins . views identifier ix p

    absolute g
      = over geometry . fmap
      $ inline (g ^. space)



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



rebuildHyperedges :: NetGraph -> NetGraph
rebuildHyperedges = set nets <$> generateHyperedges . view gates <*> id



generateHyperedges :: Foldable f => f Gate -> HashMap Identifier Net
generateHyperedges gs = HashMap.fromListWith (<>)
  [ (i, Net i mempty mempty (pure gate) (HashMap.singleton (gate ^. number) [pin]))
  | gate <- toList gs
  , (contact, i) <- HashMap.toList $ gate ^. wires
  , let pin = def & identifier .~ contact
  ]
{-# INLINABLE generateHyperedges #-}



region :: (Int -> Int) -> (Int -> Int) -> NetGraph -> NetGraph
region f g top = top &~ do
    supercell %= (over pins . fmap . over geometry . fmap) (bimap f g)
    supercell %= (over tracks . fmap) (bimap (over stabs (IntSet.map f)) (over stabs (IntSet.map g)))
    gates %= (fmap . over space) (bimap f g)
    nets %= (fmap . over contacts . fmap . fmap . over geometry . fmap) (bimap f g)
    nets %= (fmap . over netSegments . fmap) (bimap f g)



netGraphArea :: NetGraph -> Component' l Int
netGraphArea top
  = castLayer
  $ coarseBoundingBox (view space <$> view gates top) <> coarseBoundingBox (outerRim top)



outerRim :: NetGraph -> HashMap Identifier (Component' Layer Int)
outerRim
  = fmap (foldMap (coarseBoundingBox . polygon) . view geometry)
  . view (supercell . pins)



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

