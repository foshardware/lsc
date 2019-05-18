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
fromDEF (DEF options area _ cs ps ns) = def &~ do
    identifier .= identifierFrom options
    gates .= V.fromList (fromComponent sc <$> cs)
    where sc = scaleFrom options


fromComponent :: Double -> DEF.Component -> Gate
fromComponent sc (Component i j placed) = def &~ do
    identifier .= i
    geometry .= fromPlaced sc placed



fromPlaced :: Double -> Maybe Placed -> Path
fromPlaced sc (Just (Placed (x, y) _)) =
  [ Layered
    (ceiling $ x * sc)
    (ceiling $ y * sc)
    (ceiling $ x * sc)
    (ceiling $ y * sc)
    [Metal2, Metal3]
  ]
fromPlaced _ _ = mempty



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

