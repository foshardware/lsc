{-# LANGUAGE OverloadedStrings #-}

module LSC.DEF
  ( module Language.DEF.Builder
  , module Language.DEF.Parser
  , fromDEF, toDEF
  ) where

import Control.Lens
import Data.Default
import Data.Foldable
import Data.Maybe
import Data.Map (assocs, withoutKeys)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (pack)
import qualified Data.Vector as V

import Language.DEF.Builder
import Language.DEF.Parser (parseDEF)
import Language.DEF.Syntax as DEF

import LSC.Types as Rect



fromDEF :: DEF -> NetGraph
fromDEF (DEF options area ts cs ps ns) = def &~ do
    identifier .= identifierFrom options
    supercell .= supercellFrom sc area ts
    gates .= V.fromList (fromComponent sc <$> cs)

    where sc x = ceiling $ x * scaleFrom options

type Scale = Double -> Integer


supercellFrom :: Scale -> DieArea -> [DEF.Track] -> AbstractCell
supercellFrom sc (DieArea (x1, y1) (x2, y2)) ts = def &~ do
    routing .= (fromTrack sc <$> ts)
    geometry .= [Rect (sc x1) (sc y1) (sc x2) (sc y2)]


fromTrack :: Scale -> DEF.Track -> Either Rect.Track Rect.Track
fromTrack sc (DEF.Track "X" a ss c _) = Right $ Rect.Track (sc a) (fromIntegral ss) (sc c) mempty 
fromTrack sc (DEF.Track   _ a ss c _) = Left  $ Rect.Track (sc a) (fromIntegral ss) (sc c) mempty



fromComponent :: Scale -> DEF.Component -> Gate
fromComponent sc (Component i j placed) = def &~ do
    identifier .= j
    geometry .= fromPlaced sc placed



fromPlaced :: Scale -> Maybe Placed -> Path
fromPlaced sc (Just (Placed (x, y) ori)) =
  [ Layered (sc x) (sc y) (sc x) (sc y)
    [Metal2, Metal3] (fromText ori)
  ]
fromPlaced _ _ = mempty



fromText :: Identifier -> Orientation
fromText "N" = N
fromText "S" = S
fromText "W" = W
fromText "E" = E
fromText "FN" = FN
fromText "FS" = FS
fromText "FW" = FW
fromText "FE" = FE
fromText _ = error "undefined orientation"



scaleFrom :: [Option] -> Double
scaleFrom (Units (DistanceList x) : _) = fromIntegral x
scaleFrom (_ : xs) = scaleFrom xs
scaleFrom _ = 1



identifierFrom :: [Option] -> Identifier
identifierFrom (Design i : _) = i
identifierFrom (_ : xs) = identifierFrom xs
identifierFrom _ = "top"



toDEF :: NetGraph -> DEF
toDEF top = DEF
  (defaultOptions $ Just $ top ^. identifier)
  (dieArea $ top ^. supercell . geometry . to listToMaybe) 
  mempty
  (toList $ top ^. gates <&> toComponent)
  (toList $ top ^. supercell . pins <&> toPin)
  (toList $ withoutKeys (top ^. nets) power <&> toNet top)



scaleDown :: Integer -> Double
scaleDown n = fromIntegral $ div n 100



dieArea :: Maybe (Rect.Component l Integer) -> DieArea
dieArea (Just p) = DieArea (scaleDown $ p^.l, scaleDown $ p^.b) (scaleDown $ p^.r, scaleDown $ p^.t)
dieArea _ = DieArea (0, 0) (0, 0)



power :: Set Identifier
power = Set.fromList ["gnd", "vdd"]


enumeratedGate :: Gate -> Identifier
enumeratedGate g = g ^. identifier <> "_" <> pack (g ^. number . to show)


toComponent :: Gate -> DEF.Component
toComponent g = Component (enumeratedGate g) (g ^. identifier) (Just $ Placed p "N")
  where
      p = maybe (0, 0) (\x -> (scaleDown $ x^.l, scaleDown $ x^.b)) $ g ^. geometry . to listToMaybe



toNet :: NetGraph -> Rect.Net -> DEF.Net
toNet top n = DEF.Net (n ^. identifier) $

    [ Left (n ^. identifier)
    | elem (n ^. identifier) (top ^. supercell . pins <&> view identifier)
    ] ++

    [ Right (enumeratedGate g, p ^. identifier)
    | (i, ps) <- n ^. contacts . to assocs
    , p <- ps
    , g <- toList $ top ^. gates ^? ix i
    ]


toPin :: Rect.Pin -> DEF.Pin
toPin pin = DEF.Pin (pin ^. identifier)
    (Just $ pin ^. identifier)
    (Just $ Layer "metal1" (0, 0) (1, 1))
    (listToMaybe [ Placed (scaleDown $ p^.l, scaleDown $ p^.b) "N" | p <- pin ^. ports ])

