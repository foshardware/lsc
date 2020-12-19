-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module LSC.NetGraph where

import Control.Applicative
import Control.Lens hiding (imap)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.ST
import Data.ByteString.Base16
import Data.Default
import Data.Foldable
import Data.Function
import Data.Hashable
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.IntMap as M
import qualified Data.IntSet as S
import Data.List (sort, sortOn, groupBy)
import Data.List.Split (wordsBy)
import Data.Maybe
import Data.Monoid
import Data.Serialize.Put
import Data.Text (unpack)
import Data.Text.Encoding
import Data.Matrix (Matrix, nrows, ncols, getElem, getMatrixAsVector)
import Data.Vector (Vector, (!), imap, filter, backpermute)
import Data.Vector.Unboxed (unsafeFreeze)
import Data.Vector.Unboxed.Mutable (new, write)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import Prelude hiding (lookup, filter)

import LSC.Entropy
import LSC.Types



boundingBox :: (Foldable f, Integral n, Bounded n) => f (Component l n) -> Component l n
boundingBox = foldMap' implode 
{-# SPECIALIZE boundingBox :: [Component l Int] -> Component l Int #-}
{-# INLINABLE boundingBox #-}



coarseBoundingBox :: (Foldable f, Ord n, Bounded n) => f (Component l n) -> Component l n
coarseBoundingBox = foldMap' id
{-# SPECIALIZE coarseBoundingBox :: [Component l Int] -> Component l Int #-}
{-# INLINABLE coarseBoundingBox #-}



hpwl :: Net -> Int
hpwl = liftA2 (+) width height . foldMap' (implode . view space) . view members



hpwlDelta :: Foldable f => NetGraph -> f Gate -> Int
hpwlDelta top gs = sum
    [ width after - width before + height after - height before
    | net <- hyperedges top gs
    , let before = foldMap' (implode . view space)
                            (view members net)
    , let after  = foldMap' (\ g -> maybe (g ^. space . to implode) (implode . view space) $ find (== g) gs)
                            (view members net)
    ]
{-# SPECIALIZE hpwlDelta :: NetGraph -> [Gate] -> Int #-}
{-# INLINABLE hpwlDelta #-}



optimalRegion :: (Foldable f, Foldable g) => f (g Gate) -> Component l Int
optimalRegion f
    | all null f
    = error "optimalRegion: empty nodes"
optimalRegion f
    = Rect x1 y1 x2 y2
    where
        boxes = foldr ((:) . foldMap' (implode . view space)) [] f
        [x1, x2] = medianElements $ sort $ foldMap (\ box -> [box ^. l, box ^. r]) boxes
        [y1, y2] = medianElements $ sort $ foldMap (\ box -> [box ^. b, box ^. t]) boxes
{-# INLINE optimalRegion #-}



hyperedges :: Foldable f => NetGraph -> f Gate -> [Net]
hyperedges top gs = unstableUnique
    [ n
    | g <- toList gs
    , k <- toList $ view wires g
    , n <- toList $ top ^. nets ^? ix k
    ]
{-# SPECIALIZE hyperedges :: NetGraph -> [Gate] -> [Net] #-}
{-# INLINABLE hyperedges #-}



adjacentByPin :: NetGraph -> Gate -> HashMap Identifier (Vector Gate)
adjacentByPin top g
    = foldMap (filter (/= g) . view members) . (view nets top ^?) . ix <$> view wires g



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
      [ Rect x y (succ x) (succ y)
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

  debug
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

  debug
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
getSegments gs
    = gs
    & toList
    & sortOn (view (space . b))
    & groupBy ((==) `on` view (space . b))
   <&> sortOn (view space)
   <&> wordsBy (view fixed)
    & join
   <&> V.fromList



getRows :: Vector Gate -> [Vector Gate]
getRows gs
    = gs
    & toList
    & sortOn (view (space . b))
    & groupBy ((==) `on` view (space . b))
   <&> sortOn (view space)
   <&> V.fromList



gateOverlap :: Gate -> Gate -> Bool
gateOverlap i j
    = or
    [ i ^. space . l == j ^. space . l
    , i ^. space . l < j ^. space . l && i ^. space . r > j ^. space . l
    , j ^. space . l < i ^. space . l && j ^. space . r > i ^. space . l
    ] && or
    [ i ^. space . b == j ^. space . b
    , i ^. space . b < j ^. space . b && i ^. space . t > j ^. space . b
    , j ^. space . b < i ^. space . b && j ^. space . t > i ^. space . b
    ]



inlineGeometry :: NetGraph -> LSC NetGraph
inlineGeometry top = pure $ rebuildEdges $ top &~ do
 
    gates .= gs

    where

      gs = set number `imap` V.concat
        [ s ^. gates <&> project (view space g)
        | g <- toList $ top ^. gates
        , s <- toList $ HashMap.lookup (g ^. identifier) (top ^. subcells)
        ]

      project :: Component Layer Int -> Gate -> Gate
      project p = space %~ \ x -> x & l +~ p^.l & b +~ p^.b & t +~ p^.b & r +~ p^.l



gateGeometry :: NetGraph -> LSC NetGraph
gateGeometry top = do

  cells <- view stdCells <$> technology

  let fh = maximum $ snd . view dims <$> cells
      fw = maximum $ top ^. supercell . rows <&> view granularity

  let expand g | g ^. feedthrough = g & space %~ \ x -> x & r .~ x^.l + fw & t .~ x^.b + fh
      expand g = g & space %~ maybe id drag (cells ^? views identifier ix g . dims)
      drag (w, h) p = p & r .~ view l p + w & t .~ view b p + h

  pure $ top &~ do
      gates %= fmap expand



pinGeometry :: NetGraph -> LSC NetGraph
pinGeometry top = do

  cells <- view stdCells <$> technology

  let mark = contacts %~ HashMap.mapWithKey (\ i ps -> align (view gates top ^? ix i) <$> ps)

      align (Just g) p | g ^. feedthrough = p & geometry .~ pure (g ^. space)

      align (Just g) p = maybe p (absolute g)
          $ preview (ix (p ^. identifier)) =<< cells ^? ix (g ^. identifier) . pins

      align  Nothing p = p

      absolute g = geometry
          %~ fmap (l +~ (g ^. space . l))
           . fmap (r +~ (g ^. space . l))
           . fmap (b +~ (g ^. space . b))
           . fmap (t +~ (g ^. space . b))

  pure $ top &~ do
      nets %= fmap mark



perturbGates :: NetGraph -> LSC NetGraph
perturbGates top = do
    let getVector = randomPermutation (top ^. gates . to length) :: Gen s -> ST s Permutation
    v <- liftIO $ nonDeterministic getVector
    pure $ top &~ do
        gates .= set number `imap` backpermute (top ^. gates) v



assignCellsToRows :: NetGraph -> NetGraph
assignCellsToRows top
    | top ^. supercell . rows . to null
    = top
assignCellsToRows top
    = top &~ do
        gates %= fmap (space %~ (relocateB =<< closest))
    where
      rs = top ^. supercell . rows
      closest s
          = fst
          $ minimumBy (compare `on` \ (y, _) -> abs (s ^. b - y))
          $ catMaybes [M.lookupGE (s ^. b) rs, M.lookupLE (s ^. b) rs]


assignCellsToColumns :: NetGraph -> NetGraph
assignCellsToColumns top
    = top &~ do
        gates %= fmap (space %~ (relocateL =<< closest))
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
{-# SPECIALIZE generateEdges :: Vector Gate -> HashMap Identifier Net #-}
{-# INLINABLE generateEdges #-}


componentMap :: (Component Layer Int -> Component Layer Int) -> NetGraph -> NetGraph
componentMap f top = top &~ do
    gates %= fmap (space %~ f)
    nets %= fmap (contacts %~ fmap (fmap (geometry %~ fmap f)))
    supercell %= over pins (fmap (geometry %~ fmap f))



netGraphArea :: NetGraph -> Component l Int
netGraphArea top
    = component
    $ coarseBoundingBox (view space <$> view gates top) <> coarseBoundingBox (outerRim top)



outerRim :: NetGraph -> HashMap Identifier (Component l Int)
outerRim = fmap (component . coarseBoundingBox . view geometry) . view (supercell . pins)



treeStructure :: NetGraph -> NetGraph
treeStructure top = top & subcells .~ foldr collect mempty (top ^. gates)
  where
    scope = HashMap.fromList [ (x ^. identifier, x) | x <- flatten subcells top ]
    collect g a = maybe a (descend a) $ HashMap.lookup (g ^. identifier) scope
    descend a n = HashMap.insert (n ^. identifier) (n & subcells .~ foldr collect mempty (n ^. gates)) a



flatten :: Foldable f => Getter a (f a) -> a -> [a]
flatten descend netlist
  = netlist
  : join [ flatten descend model | model <- toList $ netlist ^. descend ]



netGraphStats :: NetGraph -> String
netGraphStats top = concat
  [ unwords
    [ top ^. identifier . to unpack <> ":"
    , unpack $ goedelIdentifier $ hash top, "goedel,"
    , top ^. subcells . to length . to show, "subcells,"
    , top ^. supercell . pins . to length . to show, "pins,"
    , top ^. gates ^. to length . to show, "gates,"
    , top ^. nets ^. to length . to show, "nets"
    ]
  , unlines [ mempty | start ]
  , unlines
      [ netGraphStats n
      | n <- top ^. subcells & HashMap.elems
      ]
  , unlines [ "" | start ]
  , unlines [ top ^. subcells . to length . to show
             <> " subcells." | start ]
  , unlines [ show (S.size $ S.fromList $ top ^. subcells & HashMap.elems <&> hash)
             <> " distinct subcells." | start ]
  ] where
      start = not $ top ^. subcells . to null


goedelIdentifier :: Int -> Identifier
goedelIdentifier = decodeUtf8 . encode . runPut . putInt64be . fromIntegral


gateGoedel :: HashMap Identifier Int -> Gate -> Int
gateGoedel ns g = hash [ (HashMap.lookup w ns, view identifier g) | w <- g ^. wires & HashMap.elems ]



leaves :: NetGraph -> [NetGraph]
leaves top | top ^. subcells . to null = pure top
leaves top = join $ top ^. subcells & HashMap.elems <&> leaves

