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
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.IntMap as M
import qualified Data.IntSet as S
import Data.List (sort, sortOn, groupBy)
import Data.List.Split (wordsBy)
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



coarseBoundingBox :: (Foldable f, Functor f, Ord n) => f (Component l n) -> Component l n
coarseBoundingBox cs | null cs = error "coarseBoundingBox: no components"
coarseBoundingBox cs = Rect
    (minimum $ view l <$> cs)
    (minimum $ view b <$> cs)
    (maximum $ view r <$> cs)
    (maximum $ view t <$> cs)
{-# SPECIALIZE coarseBoundingBox ::        [Component l Int] -> Component l Int #-}
{-# SPECIALIZE coarseBoundingBox :: Vector (Component l Int) -> Component l Int #-}



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



hpwl :: Vector Gate -> Net -> Int
hpwl  _ n | elem (n ^. identifier) ["clk", "CLK"] || elem (n ^. identifier) power = 0
hpwl gs n = width p + height p
  where
    p = boundingBox $ catMaybes
      [ gs ^? ix i . space 
      | i <- n ^. contacts . to HashMap.keys
      ]


hpwlDelta :: Foldable f => NetGraph -> f Gate -> Int
hpwlDelta top gs
  = sum (width  <$> qs) - sum (width  <$> ps)
  + sum (height <$> qs) - sum (height <$> ps)
  where
    ns = HashSet.fromList
      [ k
      | ks <- views wires HashMap.elems <$> toList gs
      , k <- ks
      , k /= "clk", k /= "CLK", not $ k `elem` power
      ]
    ps =
      [ boundingBox $ fmap (view space) $ catMaybes $ flip preview (top ^. gates) . ix <$> is
      | k <- toList ns
      , n <- toList $ top ^. nets ^? ix k
      , let is = views contacts HashMap.keys n
      ]
    qs =
      [ boundingBox $ fmap (view space) $ mappend hs $ catMaybes $ flip preview (top ^. gates) . ix <$> is
      | k <- toList ns
      , n <- toList $ top ^. nets ^? ix k
      , let is = [ i | i <- views contacts HashMap.keys n, all ((/= i) . view number) gs ]
      , let hs = join [ [ g | g <- toList gs, view number g == h ] | h <- views contacts HashMap.keys n ]
      ]
{-# SPECIALIZE hpwlDelta :: NetGraph -> Vector Gate -> Int #-}
{-# SPECIALIZE hpwlDelta :: NetGraph -> [Gate] -> Int #-}



optimalRegionCenter :: (Foldable f, Foldable g) => f (g Gate) -> (Int, Int)
optimalRegionCenter f = (median xs, median ys)
    where
      ns = fmap (view space) . toList <$> toList f
      xs = sort $ join [ [box ^. l, box ^. r] | box <- boundingBox <$> ns ]
      ys = sort $ join [ [box ^. b, box ^. t] | box <- boundingBox <$> ns ]
{-# SPECIALIZE optimalRegionCenter :: [[Gate]] -> (Int, Int) #-}



connectedIndices :: NetGraph -> Gate -> [[Int]]
connectedIndices top g =
    [ cs
    | net <- catMaybes
      [ top ^. nets ^? ix x . contacts . to HashMap.keys
      | x <- toList $ g ^. wires
      , x /= "clk", x /= "CLK", not $ x `elem` power
      ]
    , let cs = [ i | i <- net, i /= g ^. number, i >= 0 ]
    , not $ null cs
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


-- significantHwpl: improvement in second argument is GT
--
significantHpwl :: NetGraph -> NetGraph -> Ordering
significantHpwl m n = compare 0 $ sw n - sw m -- + div (sw n) 100000
    where sw q = sum $ hpwl (view gates q) <$> view nets q



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
   <&> sortOn (negate . length . view wires)
   <&> sortOn (view space)
   <&> V.fromList



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
      | Just sc <- view stdCells tech ^? ix (view identifier g)
      = g & vdd .~ view vdd sc & gnd .~ view gnd sc  
    vddGnd _ g
      | Just sc <- view subcells netlist ^? ix (view identifier g) <&> view supercell
      = g & vdd .~ view vdd sc & gnd .~ view gnd sc
    vddGnd _ g = g

    createNets tech = HashMap.fromListWith mappend
      [ (net, Net net mempty (HashMap.singleton (gate ^. number) [pin]))
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
rebuildEdges nodes = HashMap.fromListWith (<>)
    [ (net, Net net mempty (HashMap.singleton (gate ^. number) [pin]))
    | gate <- toList nodes
    , (contact, net) <- gate ^. wires & HashMap.toList
    , let pin = def & identifier .~ contact
    ]
{-# SPECIALIZE rebuildEdges :: Matrix Gate -> HashMap Identifier Net #-}
{-# SPECIALIZE rebuildEdges :: Vector Gate -> HashMap Identifier Net #-}


leaves :: NetGraph -> [NetGraph]
leaves top | top ^. subcells . to null = pure top
leaves top = join $ top ^. subcells & HashMap.elems <&> leaves

