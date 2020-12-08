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
import Data.List (sortOn, groupBy)
import Data.List.Split (wordsBy)
import Data.Maybe
import Data.Serialize.Put
import Data.Text (unpack)
import Data.Text.Encoding
import Data.Matrix (Matrix, nrows, ncols, getElem, getMatrixAsVector)
import Data.Vector (Vector, (!), imap, filter)
import Data.Vector.Unboxed (unsafeFreeze)
import Data.Vector.Unboxed.Mutable (new, write)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import Prelude hiding (lookup, filter)

import LSC.Types



boundingBox :: (Foldable f, Integral n, Bounded n) => f (Component l n) -> Component l n
boundingBox = foldMap' center
{-# SPECIALIZE boundingBox :: [Component l Int] -> Component l Int #-}



coarseBoundingBox :: (Foldable f, Ord n, Bounded n) => f (Component l n) -> Component l n
coarseBoundingBox = foldMap' id



hpwl :: Vector Gate -> Net -> Int
hpwl  _ net | control $ net ^. identifier = 0
hpwl gs net = width box + height box
  where box = foldMap' (\ i -> gs ! i ^. space . to center) (view members net)



hpwlDelta :: Foldable f => NetGraph -> f Gate -> Int
hpwlDelta top gs = sum

    [ width  after - width  before
    + height after - height before

    | net <- hyperedges top gs

    , let before = foldMap' (\ i -> view gates top ! i ^. space . to center) (view members net)

    , let after = foldMap' (\ i -> case find ((== i) . view number) gs of
                                       Nothing -> view gates top ! i ^. space . to center
                                       Just g  -> g ^. space . to center) (view members net)
    ]
{-# SPECIALIZE hpwlDelta :: NetGraph -> [Gate] -> Int #-}



optimalRegionCenter :: (Foldable f, Foldable g) => f (g Gate) -> (Int, Int)
optimalRegionCenter f = (median xs, median ys)
    where
      ns = fmap (view space) . toList <$> toList f
      xs = join [ [box ^. l, box ^. r] | box <- boundingBox <$> ns ]
      ys = join [ [box ^. b, box ^. t] | box <- boundingBox <$> ns ]
{-# SPECIALIZE optimalRegionCenter :: [[Gate]] -> (Int, Int) #-}



hyperedges :: Foldable f => NetGraph -> f Gate -> [Net]
hyperedges top gs = uniqueBy compare $ catMaybes $
    [ top ^. nets ^? ix k
    | ks <- view wires <$> toList gs
    , k <- toList ks
    , not $ control k
    ]
{-# SPECIALIZE hyperedges :: NetGraph -> [Gate] -> [Net] #-}



adjacentByNet :: NetGraph -> Gate -> [Vector Gate]
adjacentByNet top g =
    [ (view gates top !) <$> V.filter (/= view number g) (view members net)
    | k <- toList $ view wires g
    , not $ control k
    , net <- toList $ top ^. nets ^? ix k
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
 
  let box = coarseBoundingBox $ view space <$> gs

  let area = head (top ^. supercell . geometry)
      pivot = div (area ^. r + area ^. l) 2
  let (xs, ys) = V.partition ((<= pivot) . centerX) (view space <$> V.filter (views fixed not) gs)

  debug
    [ unpack (view identifier top) ++ " layout area: " ++ show (width box, height box)
    , unpack (view identifier top) ++ " sum of hpwl: " ++ show (sum $ hpwl gs <$> ns)
    , unpack (view identifier top) ++ " gate count: "  ++ show (length gs)
    , unpack (view identifier top) ++ " row balance: "
      ++ show (sum $ width <$> xs) ++ " | " ++ show (sum $ width <$> ys)
    ]



significantHpwl :: NetGraph -> NetGraph -> Ordering
significantHpwl m n = compare (sw m `div` x) (sw n `div` x)
    where sw q = sum $ hpwl (view gates q) <$> view nets q
          x = sw m `div` 20000



getSegments :: Vector Gate -> [Vector Gate]
getSegments gs
    = gs
    & toList
    & sortOn (view $ space . b)
    & groupBy ((==) `on` view (space . b))
   <&> sortOn (view space)
   <&> wordsBy (view fixed)
    & join
   <&> V.fromList



getRows :: Vector Gate -> [Vector Gate]
getRows gs
    = gs
    & toList
    & sortOn (view $ space . b)
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
inlineGeometry top = pure $ top &~ do
 
    gates .= gs
    nets .= ns

    where

      ns = rebuildEdges gs

      gs = set number `imap` V.concat
        [ s ^. gates <&> project (view space g)
        | g <- toList $ top ^. gates
        , s <- toList $ HashMap.lookup (g ^. identifier) (top ^. subcells)
        ]

      project :: Component Layer Int -> Gate -> Gate
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
assignCellsToRows top = pure $ top &~ do
    gates %= fmap (space %~ (relocateB =<< closest))
    where
      rs = top ^. supercell . rows
      closest s
          = fst
          $ minimumBy (compare `on` \ (y, _) -> abs (s ^. b - y))
          $ catMaybes [M.lookupGE (s ^. b) rs, M.lookupLE (s ^. b) rs]


assignCellsToColumns :: NetGraph -> LSC NetGraph
assignCellsToColumns top = pure $ top &~ do
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



power :: Identifier -> Bool
power "gnd" = True
power "vdd" = True
power "vss" = True
power "vcc" = True
power "GND" = True
power "VDD" = True
power "VSS" = True
power "VCC" = True
power _ = False


control :: Identifier -> Bool
control "clk" = True
control "CLK" = True
control i = power i



contactGeometry :: NetGraph -> LSC NetGraph
contactGeometry netlist = do

  tech <- technology

  pure $ netlist &~ do
      gates %= fmap (vddGnd tech)
      nets  .= createNets tech

  where

    vddGnd tech g
      | Just sc <- view stdCells tech ^? ix (view identifier g)
      = g & vdd .~ view vdd sc & gnd .~ view gnd sc  
    vddGnd _ g
      | Just sc <- view subcells netlist ^? ix (view identifier g) <&> view supercell
      = g & vdd .~ view vdd sc & gnd .~ view gnd sc
    vddGnd _ g = g

    createNets tech = HashMap.fromListWith mappend
      [ (net, Net net mempty (pure (gate ^. number)) (HashMap.singleton (gate ^. number) [pin]))
      | gate <- toList $ netlist ^. gates
      , (contact, net) <- gate ^. wires & HashMap.toList
      , let key = gate ^. identifier
      , let scope = HashMap.lookup contact
      , pin <- toList
            $ join (tech ^. stdCells ^? ix key . pins . to scope)
          <|> join (netlist ^. subcells ^? ix key . supercell . pins . to scope)
      ]



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



rebuildEdges :: Foldable f => f Gate -> HashMap Identifier Net
rebuildEdges ns = HashMap.fromListWith (<>)
    [ (net, Net net mempty (pure (gate ^. number)) (HashMap.singleton (gate ^. number) [pin]))
    | gate <- toList ns
    , (contact, net) <- gate ^. wires & HashMap.toList
    , let pin = def & identifier .~ contact
    ]
{-# SPECIALIZE rebuildEdges :: Matrix Gate -> HashMap Identifier Net #-}
{-# SPECIALIZE rebuildEdges :: Vector Gate -> HashMap Identifier Net #-}


leaves :: NetGraph -> [NetGraph]
leaves top | top ^. subcells . to null = pure top
leaves top = join $ top ^. subcells & HashMap.elems <&> leaves

