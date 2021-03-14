-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

module LSC.CellFlipping where

import Control.Applicative
import Control.Lens hiding (_10, _11)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.ST
import Control.Monad.State

import Data.Bifoldable
import Data.Default
import Data.Foldable
import Data.List (sortOn)
import Data.Maybe
import Data.Ratio
import Data.Set (Set, insert, member, notMember)
import Data.STRef
import Data.Text (unpack)
import Data.Vector ((!), slice, filter, zipWith, accum, replicate)

import Prelude hiding (filter, zipWith, replicate)

import LSC.Cartesian
import LSC.Component
import LSC.Integer
import LSC.NetGraph
import LSC.Polygon
import LSC.SegmentTree
import LSC.Trace
import LSC.Types




type Program = LP PseudoBool Rational


data PseudoBool
  = G Number
  | Lambda Int
  deriving (Eq, Ord)

instance Show PseudoBool where
    show (G n) = "g_" ++ show n
    show (Lambda n) = "lambda_" ++ show n



type Alpha = Int

type Beta = (Int, Int, Int, Int)

_00, _10, _01, _11 :: Lens' Beta Int
_00 = _1
_10 = _2
_01 = _3
_11 = _4


type Delta = State Beta ()



data Quadratic a b
  = Degree0 b
  | Degree1 a b b
  | Degree2 a a b b b b
  deriving Show


quadraticFormula :: [Gate] -> Beta -> Quadratic PseudoBool Int
quadraticFormula (i : j : _) beta
  = Degree2
    (i ^. number . to G)
    (j ^. number . to G)
    (beta ^. _00)
    (beta ^. _10 - beta ^. _00)
    (beta ^. _01 - beta ^. _10)
    (beta ^. _11 - beta ^. _10 - beta ^. _01 + beta ^. _00)
quadraticFormula (i : _) beta
  = Degree1
    (i ^. number . to G)
    (beta ^. _00)
    (beta ^. _10 - beta ^. _00)
quadraticFormula _ beta
  = Degree0 (beta ^. _00)



halfRoof :: Quadratic PseudoBool Int -> LinFunc PseudoBool Rational
halfRoof (Degree0 _b)
  = mempty
halfRoof (Degree1 i _b0 b1)
  = linearCombination
  [ (fromIntegral b1, i)
  ]
halfRoof (Degree2 i j _b00 b10 b01 b11)
  = linearCombination
  [ (fromIntegral b10, i)
  , (fromIntegral b01, j)
  , (fromIntegral b11 % 2, i)
  , (fromIntegral b11 % 2, j)
  ]




cellFlipping :: NetGraph -> LSC NetGraph
cellFlipping top = do

    info ["Cell flipping"]

    assume "globalDetermineNetSegments: gates are not aligned to rows!"
      $ all (isJust . getRow top) $ top ^. gates

    let program = obtainProgram top

    liftIO $ debugLP program $ top ^. identifier . to unpack ++ "_cell_flipping.lp"

    result <- liftIO $ getSolutionVector program

    case result of

        Nothing -> do

            warning ["No result in cell flipping"]
            pure top

        Just (objective, vector) -> do

            let solution = toEnum <$> views gates (slice 0 . length) top vector

            debug $ "Cells flipped:" : take 800
              [ show i ++ " - " ++ g ^. identifier . to unpack ++ ": " ++ show o
              | g <- toList $ top ^. gates
              , let i = g ^. number
              , let o = solution ^. ix i <> g ^. space . orientation
              , solution ^. ix i /= N
              ]

            info
              [ "Cell flipping objective: " ++ show objective
              , "Number of cells flipped: " ++ show (length $ filter (/= N) solution)
              ]

            pure
              $ top &~ do
                gates %= zipWith (over space . (<>~) orientation) solution




type Edge = (Vertex, Vertex)

data Vertex = Vertex
  { channel  :: Int
  , net      :: Net
  , gate     :: Gate
  , pin      :: Pin
  }

instance Eq Vertex where
    u == v = channel u == channel v
      && gate u ^. number == gate v ^. number
      && pin u ^. identifier == pin v ^. identifier

instance Ord Vertex where
    compare u v = compare (channel u) (channel v)
      <> compare (gate u ^. number) (gate v ^. number)
      <> compare (pin u ^. identifier) (pin v ^. identifier)



edge :: Gate -> Net -> Pin -> Int -> Edge
edge g n p i = (Vertex (pred i) n g p, Vertex (succ i) n g p)


abscissa :: Vertex -> (Alpha, Alpha)
abscissa = computeAlpha <$> gate <*> pin



obtainProgram :: NetGraph -> Program
obtainProgram top
  = minimize
  $ do

    setObjective $ linearCombination [ (1, Lambda q) | q <- toList $ imap const _A ]

    ifor_ _A $ \ q density -> do

      linearCombination [(1, Lambda q)] `geq` foldMap (halfRoof . formula density (_J ! q)) density

    mapM_ boolean $ G . view number <$> top ^. gates
    mapM_ integer $ Lambda <$> imap const _A

  where

    _A = liftA2 constructSegmentTree (endpoints . bifold) fst <$> zipWith (,) alphas alphas'

    _J = zipWith (foldl (\ s (i, c) -> adjust i (c:) s))
         (skeleton . endpoints <$> zipWith (++) alphas alphas')
         (zipWith zip ranges cells)

    alphas  = map (bimap (fst . abscissa) (fst . abscissa)) <$> channels
    alphas' = map (bimap (snd . abscissa) (snd . abscissa)) <$> channels


    formula density segment alpha
      = flip trace()
      $ quadraticFormula xs
      $ execState (computeDelta density alpha)
      $ runST (computeBeta _P phi xs alpha)
      where xs = stab alpha segment


    _P :: Gate -> Set Vertex
    _P i = vertices ^. ix (i ^. number)

    phi :: Gate -> [Edge]
    phi i = edges ^. ix (i ^. number)

    vertices
      = accum (flip insert) (replicate (top ^. gates . to length) mempty)
      $ liftA2 (,) (view number . gate) id
         <$> foldMap biList (fold channels)

    edges
      = accum (flip (:)) (replicate (top ^. gates . to length) mempty)
      . foldMap (\ (u, v) -> [(gate u ^. number, (u, v)), (gate v ^. number, (u, v))])
      $ fold channels


    cells
      = map getDistinctNumber
      . unstableUnique
      . map DistinctNumber
      . map gate
      . foldMap biList
        <$> channels

    ranges = map (liftA2 (,) (succ . view (space . l)) (pred . view (space . r))) <$> cells


    channels
      = accum (flip (:)) (replicate (top ^. supercell . rows . to length . to succ) [])
      . map (liftA2 (,) (channel . fst) id)
      . fold
      $ foldMap (liftA2 zip id tail . sortOn abscissa)
      . groupOn channel
      . sortOn channel
      . foldMap biList
      . liftA2 map builtInEdge (verticesOf top)
        <$> top ^. nets

    builtInEdge n (g, p) = edge g n p (maybe 0 (view number) (getRow top g))




computeAlpha :: Gate -> Pin -> (Alpha, Alpha)
computeAlpha g p
  = (alpha, alpha')
  where
    alpha = centerX $ coarseBoundingBox $ p ^. geometry <&> containingBox
    alpha' = g ^. space . l + g ^. space . r - alpha



computeBeta
  :: (Gate -> Set Vertex)
  -> (Gate -> [Edge])
  -> [Gate]
  -> Alpha
  -> ST s Beta

computeBeta _P phi xs alpha
  = do

    let i = head $ xs ++ [def]
        j = head $ drop 1 xs ++ [def]

    b00 <- newSTRef 0
    b10 <- newSTRef 0
    b01 <- newSTRef 0
    b11 <- newSTRef 0

    for_
      [ (False, False, b00)
      ,  (True, False, b10)
      , (False,  True, b01)
      ,  (True,  True, b11)
      ]
      $ \ (xi, xj, beta) -> for_ (phi i <> phi j)
      $ \ (u, v) -> do

        yl <- newSTRef maxBound
        zl <- newSTRef minBound

        for_ [u, v] $ \ p -> do

            let (a, a') = abscissa p

            if p `notMember` _P i && p `notMember` _P j
            then do
              modifySTRef' yl $ min a
              modifySTRef' zl $ max a
            else do
              when (not xi && p `member` _P i || not xj && p `member` _P j)
                $ do
                  modifySTRef' yl $ min a
                  modifySTRef' zl $ max a
              when (xi && p `member` _P i || xj && p `member` _P j)
                $ do
                  modifySTRef' yl $ min a'
                  modifySTRef' zl $ max a'

        x1 <- readSTRef yl
        x2 <- readSTRef zl
        when (x1 <= alpha && alpha <= x2)
          $ do
            modifySTRef' beta succ

    (,,,)
      <$> readSTRef b00
      <*> readSTRef b10
      <*> readSTRef b01
      <*> readSTRef b11



computeDelta :: SegmentTree Alpha -> Alpha -> Delta
computeDelta segment alpha
  = do
    beta <- get
    let delta = densityOn alpha segment - beta ^. _00
    _00 += delta
    _10 += delta
    _01 += delta
    _11 += delta

