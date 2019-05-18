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

import Language.DEF.Builder
import Language.DEF.Parser (parseDEF)
import Language.DEF.Syntax as DEF

import LSC.Types as Rect



fromDEF :: DEF -> NetGraph
fromDEF = const def



toDEF :: NetGraph -> DEF
toDEF top = DEF
  (defaultOptions $ Just $ top ^. identifier)
  (dieArea $ top ^. supercell . geometry . to listToMaybe) 
  mempty
  (toList $ top ^. gates <&> fromComponent)
  (toList $ top ^. supercell . pins <&> fromPin)
  (toList $ withoutKeys (top ^. nets) power <&> fromNet top)



scaleDown :: Integer -> Double
scaleDown n = fromIntegral $ div n 100



dieArea :: Maybe (Rect.Component l Integer) -> DieArea
dieArea (Just p) = DieArea (scaleDown $ p^.l, scaleDown $ p^.b) (scaleDown $ p^.r, scaleDown $ p^.t)
dieArea _ = DieArea (0, 0) (0, 0)



power :: Set Identifier
power = Set.fromList ["gnd", "vdd"]


enumeratedGate :: Gate -> Identifier
enumeratedGate g = g ^. identifier <> "_" <> pack (g ^. number . to show)


fromComponent :: Gate -> DEF.Component
fromComponent g = Component (enumeratedGate g) (g ^. identifier) (Just $ Placed p "N")
  where
      p = maybe (0, 0) (\x -> (scaleDown $ x^.l, scaleDown $ x^.b)) $ g ^. geometry . to listToMaybe



fromNet :: NetGraph -> Rect.Net -> DEF.Net
fromNet top n = DEF.Net (n ^. identifier) $

    [ Left (n ^. identifier)
    | elem (n ^. identifier) (top ^. supercell . pins <&> view identifier)
    ] ++

    [ Right (enumeratedGate g, p ^. identifier)
    | (i, ps) <- n ^. contacts . to assocs
    , p <- ps
    , g <- toList $ top ^. gates ^? ix i
    ]


fromPin :: Rect.Pin -> DEF.Pin
fromPin pin = DEF.Pin (pin ^. identifier)
    (Just $ pin ^. identifier)
    (Just $ Layer "metal1" (0, 0) (1, 1))
    (listToMaybe [ Placed (scaleDown $ p^.l, scaleDown $ p^.b) "N" | p <- pin ^. ports ])

