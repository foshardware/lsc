-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module LSC.DEF
  ( module Language.DEF.Builder
  , module Language.DEF.Parser
  , fromDEF, toDEF
  ) where

import Control.Lens
import Control.Monad
import Data.Bifunctor
import Data.Default
import Data.Either
import Data.Foldable
import qualified Data.HashMap.Lazy as HashMap
import Data.Maybe
import Data.Map (fromList, lookup)
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import Data.Monoid
import Data.Text (unpack)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int
import qualified Data.Vector as V
import Prelude hiding (lookup)

import Language.DEF.Builder (printDEF, builderDEF, defaultOptions)
import Language.DEF.Parser (parseDEF)
import Language.DEF.Syntax as DEF

import LSC.Component as LSC
import LSC.Types as LSC
import LSC.Polygon as LSC



fromDEF :: DEF -> NetGraph
fromDEF (DEF options _ area rs ts _ _ cs ps ns _) = def &~ do
    identifier .= identifierFrom options
    supercell .= supercellFrom area ts rs (toList ps)
    gates .= V.zipWith (set wires) paths nodes
    nets .= edges
    where

      paths = V.accum (\ m (k, v) -> HashMap.insert k v m) (mempty <$ nodes)
          [ (i, (x ^. identifier, e ^. identifier))
          | e <- toList edges
          , (i, xs) <- views contacts HashMap.toList e
          , i >= 0
          , x <- xs
          ]

      nodes = set number `imap` fmap fromComponent cs

      edges = HashMap.fromList [ (n ^. identifier, n) | n <- fromNet gate <$> toList ns ]

      gate y = maybe def id $ lookup y $ fromList [ (x, g) | (DEF.Component x _ _, g) <- toList $ V.zip cs nodes ]



fromNet :: (Ident -> Gate) -> DEF.Net -> LSC.Net
fromNet gate (DEF.Net i cs _) = LSC.Net i
    mempty
    mempty
    (V.fromList $ gate . fst <$> rights cs)
    (HashMap.fromListWith (++)
        [ (g ^. number, [def & identifier .~ p]) | (g, p) <- either (def, ) (first gate) <$> cs ])


fromLayer :: LayerName -> LSC.Layer
fromLayer "metal1" = Metal1
fromLayer "metal2" = Metal2
fromLayer "metal3" = Metal3
fromLayer "metal4" = Metal4
fromLayer "metal5" = Metal5
fromLayer "metal6" = Metal6
fromLayer "metal7" = Metal7
fromLayer "metal8" = Metal8
fromLayer "metal9" = Metal9
fromLayer "metal10" = Metal10
fromLayer _ = AnyLayer



supercellFrom :: DieArea -> [Tracks] -> [DEF.Row] -> [DEF.Pin] -> AbstractCell
supercellFrom (DieArea (x1, y1) (x2, y2)) ts rs ps = def &~ do
    geometry .= [rect (round x1) (round y1) (round x2) (round y2)]
    tracks .= fmap fromTracks ts
    rows .= IntMap.fromDistinctAscList
        ( fmap (\ (i, (y, row)) -> (y, set number i row))
        $ zip [0..] $ IntMap.assocs
        $ IntMap.fromList [ (p ^. b, p) | p <- fromRow <$> rs ]
        )
    pins .= HashMap.fromList [ (p ^. identifier, p) | p <- fmap fromPin ps ] 


fromPin :: DEF.Pin -> LSC.Pin
fromPin (DEF.Pin p _ d layer placed) = def &~ do
    identifier .= p
    dir .= fmap fromDirection d
    geometry .=
      [ fromPlaced f
        & l +~ fromIntegral x1
        & b +~ fromIntegral y1
        & r +~ fromIntegral x2
        & t +~ fromIntegral y2
        & layers .~ [fromLayer q]
        & simplePolygon
      | (Layer q (x1, y1) (x2, y2), f) <- maybeToList $ (,) <$> layer <*> placed
      ]


fromDirection :: DEF.Direction -> Dir
fromDirection DEF.Input  = In
fromDirection DEF.Output = Out
fromDirection DEF.InputOutput = InOut


fromTracks :: DEF.Tracks -> Either Track Track
fromTracks (Tracks "X" a ss c ls)
    = Right
    $ Track (IntSet.fromList $ (round a +) . (round c *) <$> [ 0 .. fromIntegral ss - 1])
            (IntSet.fromList $ fromEnum . fromLayer <$> ls)
fromTracks (Tracks "Y" a ss c ls)
    = Left
    $ Track (IntSet.fromList $ (round a +) . (round c *) <$> [ 0 .. fromIntegral ss - 1])
            (IntSet.fromList $ fromEnum . fromLayer <$> ls)
fromTracks (Tracks d _ _ _ _) = error $ "fromTracks: undefined axis " ++ unpack d


fromRow :: DEF.Row -> LSC.Row
fromRow (DEF.Row _ i x y o ss _ w _)
    = LSC.Row i (-1)
      (fromIntegral x) (fromIntegral y)
      (fromOrientation o) (fromIntegral ss) (fromIntegral w)



fromComponent :: DEF.Component -> Gate
fromComponent (DEF.Component _ j placed@(Just (Fixed _ _))) = def &~ do
    identifier .= j
    space .= foldMap fromPlaced placed
    fixed .= True
fromComponent (DEF.Component _ j placed) = def &~ do
    identifier .= j
    space .= foldMap fromPlaced placed



fromPlaced :: Placed -> Component' LSC.Layer Int
fromPlaced (Placed (x, y) ori)
    = LSC.Component (round x) (round y) (round x) (round y) mempty (fromOrientation ori)
fromPlaced (Fixed (x, y) ori)
    = LSC.Component (round x) (round y) (round x) (round y) mempty (fromOrientation ori)
fromPlaced _ = mempty 



fromOrientation :: Identifier -> Orientation
fromOrientation "N" = N
fromOrientation "S" = S
fromOrientation "W" = W
fromOrientation "E" = E
fromOrientation "FN" = FN
fromOrientation "FS" = FS
fromOrientation "FW" = FW
fromOrientation "FE" = FE
fromOrientation o = error $ "fromOrientation: undefined orientation " ++ unpack o



identifierFrom :: [Option] -> Identifier
identifierFrom (Design i : _) = i
identifierFrom (_ : xs) = identifierFrom xs
identifierFrom _ = "top"



toDEF :: Double -> NetGraph -> DEF
toDEF scale top = DEF
  (filter units (defaultOptions $ Just $ top ^. identifier) ++ [Units $ DistanceList $ round scale])
  mempty
  (dieArea $ listToMaybe $ top ^. supercell . geometry)
  (toList $ top ^. supercell . rows <&> toRow)
  (toList $ top ^. supercell . tracks <&> toTracks)
  mempty
  mempty
  (top ^. gates <&> toComponent)
  (V.fromListN (top ^. supercell . pins . to length) $ toList $ top ^. supercell . pins <&> toPin)
  (V.fromListN (top ^. nets . to length) $ toList $ view nets top <&> toNet top)
  mempty

  where

    units (Units _) = False
    units _ = True



toRow :: LSC.Row -> DEF.Row
toRow x = DEF.Row
    (toStrict $ toLazyText $ enumeratedRow (view number x))
    (view identifier x)
    (fromIntegral $ view l x) (fromIntegral $ view b x)
    (toOrientation $ view orientation x)
    (fromIntegral $ view cardinality x) 1
    (fromIntegral $ view granularity x) 0



toTracks :: Either Track Track -> DEF.Tracks
toTracks (Right tr)
  = Tracks "X"
    (fromIntegral g)
    (fromIntegral $ IntSet.size $ tr ^. stabs)
    (fromIntegral $ h - g)
    (tr ^. layers <&> toLayer)
    where g : h : _ = IntSet.elems $ tr ^. stabs
toTracks (Left tr)
  = Tracks "Y"
    (fromIntegral g)
    (fromIntegral $ IntSet.size $ tr ^. stabs)
    (fromIntegral $ h - g)
    (tr ^. layers <&> toLayer)
    where g : h : _ = IntSet.elems $ tr ^. stabs



dieArea :: Maybe (Component' l Int) -> DieArea
dieArea (Just p) = DieArea
    (fromIntegral $ p^.l, fromIntegral $ p^.b)
    (fromIntegral $ p^.r, fromIntegral $ p^.t)
dieArea _ = DieArea (0, 0) (0, 0)



enumeratedGate :: Gate -> Builder
enumeratedGate g
    =  fromText (view identifier g)
    <> singleton '_'
    <> decimal (view number g)

enumeratedRow :: Int -> Builder
enumeratedRow n
    =  fromText "ROW_"
    <> decimal n


toComponent :: Gate -> DEF.Component
toComponent g = DEF.Component
  (toStrict $ toLazyText $ enumeratedGate g)
  (g ^. identifier)
  (Just $ place $ g ^. space)
  where
    place x | g ^. fixed = Fixed (fromIntegral $ x^.l, fromIntegral $ x^.b) "N"
    place x = Placed (fromIntegral $ x^.l, fromIntegral $ x^.b) "N"



toNet :: NetGraph -> LSC.Net -> DEF.Net
toNet top n = DEF.Net (n ^. identifier)

    [ maybe (Left (p ^. identifier)) (Right . (, p ^. identifier) . toStrict . toLazyText . enumeratedGate)
    $ top ^. gates ^? ix i
    | (i, ps) <- n ^. contacts & HashMap.toList
    , p <- ps
    ]

    Nothing



toPin :: LSC.Pin -> DEF.Pin
toPin pin = DEF.Pin (pin ^. identifier)
    (Just $ pin ^. identifier)
    (pin ^. dir <&> toDirection)
    (listToMaybe [ Layer (toLayer $ last $ AnyLayer : p ^. layers) (0, 0)
                         (fromIntegral $ width p, fromIntegral $ height p)
                 | p <- polygon =<< pin ^. geometry
                 ])
    (listToMaybe [ Fixed (fromIntegral $ p^.l, fromIntegral $ p^.b)
                         (toOrientation $ p ^. orientation)
                 | p <- polygon =<< pin ^. geometry
                 ])


toOrientation :: Orientation -> Identifier
toOrientation N = "N"
toOrientation S = "S"
toOrientation W = "W"
toOrientation E = "E"
toOrientation FN = "FN"
toOrientation FS = "FS"
toOrientation FW = "FW"
toOrientation FE = "FE"


toLayer :: LSC.Layer -> LayerName
toLayer Metal1 = "metal1"
toLayer Metal2 = "metal2"
toLayer Metal3 = "metal3"
toLayer Metal4 = "metal4"
toLayer Metal5 = "metal5"
toLayer Metal6 = "metal6"
toLayer Metal7 = "metal7"
toLayer Metal8 = "metal8"
toLayer Metal9 = "metal9"
toLayer Metal10 = "metal10"
toLayer AnyLayer = "UNKNOWN"


toDirection :: Dir -> DEF.Direction
toDirection In  = DEF.Input
toDirection Out = DEF.Output
toDirection InOut = DEF.InputOutput

