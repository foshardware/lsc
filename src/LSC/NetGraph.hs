-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Pure morphisms on the netgraph model
--
module LSC.NetGraph where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.ST
import Data.Char
import Data.Default
import Data.Function
import Data.Functor.Contravariant
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.IntMap as M
import qualified Data.IntSet as S
import Data.List (sort, sortOn)
import Data.List.Split (wordsBy)
import Data.Maybe
import Data.Matrix (Matrix, nrows, ncols, getElem, getMatrixAsVector)
import qualified Data.Text as T
import Data.Vector (Vector, (!))
import Data.Vector.Unboxed (unsafeFreeze)
import Data.Vector.Unboxed.Mutable (new, write)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V

import LSC.BinarySearch
import LSC.Cartesian
import LSC.Component
import LSC.HigherOrder
import LSC.Model
import LSC.Polygon



boundingBox :: Foldable f => f (Component' l Int) -> Component' l Int
boundingBox = getBoundingBox . foldMap' implode
{-# INLINABLE boundingBox #-}



coarseBoundingBox :: Foldable f => f (Component' l Int) -> Component' l Int
coarseBoundingBox = getBoundingBox . foldMap' BoundingBox
{-# INLINABLE coarseBoundingBox #-}



hpwl :: Net -> Int
hpwl
  = liftA2 (+) width height
  . getBoundingBox
  . foldMap' (implode . view geometry)
  . view members



hpwlDelta :: Foldable f => NetGraph -> f Gate -> Int
hpwlDelta top gs = sum
  [ width after - width before + height after - height before
  | net <- hyperedges top gs
  , let before = foldMap' (implode . view geometry)
                          (view members net)
  , let after  = foldMap' (\ g -> maybe (g ^. geometry . to implode) (implode . view geometry)
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
    boxes = foldr ((:) . foldMap' (implode . view geometry)) [] f
    [x1, x2] = medianElements $ sort $ foldMap abscissae boxes
    [y1, y2] = medianElements $ sort $ foldMap ordinates boxes
{-# INLINABLE optimalRegion #-}



hyperedges :: Foldable f => NetGraph -> f Gate -> [Net]
hyperedges top gs
  = map getDistinct
  . unstableUnique
  $ [ distinct (n ^. identifier) n
    | g <- toList gs
    , k <- toList $ g ^. wires
    , n <- toList $ top ^. nets ^? ix k
    ]
{-# INLINABLE hyperedges #-}



adjacentByPin :: NetGraph -> Gate -> HashMap Identifier [Gate]
adjacentByPin top g
  = foldMap (filter (\ h -> g ^. number /= h ^. number) . view members)
  . flip preview (view nets top) . ix
    <$> view wires g



verticesByRow :: NetGraph -> Net -> [[(Gate, Pin)]]
verticesByRow top
  = groupOn (view (_1 . geometry . b))
  . sortOn (view (_1 . geometry . b))
  . verticesOf top


verticesByColumn :: NetGraph -> Net -> [[(Gate, Pin)]]
verticesByColumn top net =
  [ [ (g, p & geometry .~ [simplePolygon c])
    | c <- foldMap (columns (row ^. l) (row ^. granularity)) (p ^. geometry)
    ]
  | (g, p) <- verticesOf top net
  , row <- toList $ top ^. supercell . rows ^? ix (g ^. geometry . b)
  ]


verticesOf :: NetGraph -> Net -> [(Gate, Pin)]
verticesOf top net =
  [ (view gates top ! i, p)
  | (i, ps) <- views contacts HashMap.toList net
  , i >= 0
  , p <- ps
  ]




hpwlMatrix :: U.Vector (Int, Int) -> Net -> Int
hpwlMatrix _ n | n ^. clock = 1
hpwlMatrix m n = width p + height p
  where
    p = boundingBox
      [ rect x y (succ x) (succ y)
      | i <- views contacts HashMap.keys n 
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
flattenGateMatrix = V.filter (\ g -> g ^. number >= 0) . getMatrixAsVector



sumOfHpwlMatrix :: Matrix Gate -> Int
sumOfHpwlMatrix m = sum $ hpwlMatrix (coordsVector m) <$> generateHyperedges m




significantHpwl :: Comparison NetGraph
significantHpwl = Comparison $ \ m n ->
  let p = sum $ hpwl <$> view nets m
      q = sum $ hpwl <$> view nets n
      x = max 1 $ div p 10000 -- delta below 0.01% is equal
  in div p x `compare` div q x



getSegments :: Vector Gate -> [Vector Gate]
getSegments
  = map V.fromList
  . foldMap (wordsBy (view fixed) . sortOn (view geometry))
  . groupOn (view (geometry . b))
  . sortOn (view (geometry . b))
  . toList



getRows :: Vector Gate -> [Vector Gate]
getRows
  = map (V.fromList . sortOn (view geometry))
  . groupOn (view (geometry . b))
  . sortOn (view (geometry . b))
  . toList



getRow :: NetGraph -> Gate -> Maybe Row
getRow top g = top ^. supercell . rows ^? ix (g ^. geometry . b)



logicBlocks :: Vector Gate -> Vector Gate
logicBlocks
  = imap (set number)
  . V.filter (views feedthrough not)
  . V.filter (views fixed not)




inlineGeometry :: NetGraph -> NetGraph
inlineGeometry top
  = rebuildHyperedges
  $ top &~ do
    gates .= set number `imap` V.concat
      [ s ^. gates <&> over geometry (inline (g ^. geometry))
      | g <- toList $ top ^. gates
      , s <- toList $ top ^. subcells ^? views identifier ix g
      ]




assignCellsToRows :: NetGraph -> NetGraph
assignCellsToRows top
    | top ^. supercell . rows . to null
    = top
assignCellsToRows top
    = top & gates %~ fmap (over geometry $ relocateB =<< closest)
    where
      rs = top ^. supercell . rows
      closest s
        = fst
        $ minimumBy (compare `on` \ (y, _) -> abs (s ^. b - y))
        $ catMaybes [M.lookupGE (s ^. b) rs, M.lookupLE (s ^. b) rs]


assignCellsToColumns :: NetGraph -> NetGraph
assignCellsToColumns top
    = top & gates %~ fmap (over geometry $ relocateL =<< closest)
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
removeFeedthroughs = gates %~ imap (set number) . V.filter (views feedthrough not)



rebuildHyperedges :: NetGraph -> NetGraph
rebuildHyperedges = set nets <$> generateHyperedges . view gates <*> id



generateHyperedges :: Foldable f => f Gate -> HashMap Identifier Net
generateHyperedges gs = HashMap.fromListWith (<>)
  [ (i, Net i mempty mempty [gate] (HashMap.singleton (gate ^. number) [pin]))
  | gate <- toList gs
  , (contact, i) <- HashMap.toList $ gate ^. wires
  , let pin = def & identifier .~ contact
  ]
{-# INLINABLE generateHyperedges #-}



region :: (Int -> Int) -> (Int -> Int) -> NetGraph -> NetGraph
region f g top = top &~ do
    supercell %= (over pins . fmap . over geometry . fmap) (bimap f g)
    supercell %= (over tracks . fmap) (bimap (over stabs (S.map f)) (over stabs (S.map g)))
    gates %= (fmap . over geometry) (bimap f g)
    nets %= (fmap . over contacts . fmap . fmap . over geometry . fmap) (bimap f g)
    nets %= (fmap . over netSegments . fmap) (bimap f g)



netGraphArea :: NetGraph -> Component' l Int
netGraphArea top
  = castLayer
  $ getBoundingBox
  $ foldMap' (BoundingBox . view geometry) (view gates top) <> foldMap' BoundingBox (outerRim top)



outerRim :: NetGraph -> HashMap Identifier (Component' Layer Int)
outerRim
  = fmap (coarseBoundingBox . fmap containingBox . view geometry)
  . view (supercell . pins)


-- cheating!
clock :: Getter Net Bool
clock = identifier . to ((== "clk") . T.map toLower)



feedthroughGate :: Net -> Gate
feedthroughGate n = def &~ do
    identifier .= ("FEEDTHROUGH." <> n ^. identifier)
    feedthrough .= True
    wires .= HashMap.singleton "FD" (n ^. identifier)



gateWidth, gateHeight :: Gate -> Int
gateWidth  = width  . view geometry
gateHeight = height . view geometry



gateOverlap :: Gate -> Gate -> Bool
gateOverlap x y = areaOverlap (x ^. geometry) (y ^. geometry)



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

