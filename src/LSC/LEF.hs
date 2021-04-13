-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}

module LSC.LEF
  ( module Language.LEF.Parser
  , module Language.LEF.Syntax
  , fromLEF
  ) where

import Control.Lens
import Data.Default
import Data.Foldable
import Data.HashMap.Lazy as HashMap
import Data.Maybe

import Language.LEF.Parser (parseLEF)
import Language.LEF.Syntax

import LSC.Component as LSC
import LSC.Model
import LSC.Polygon
import LSC.Transformer



fromLEF :: LEF -> Bootstrap ()
fromLEF (LEF options _ _ _ _ _ macros) = do

  rescale $ fromIntegral $ databaseUnits options

  tech <- snapshot

  stdCells .= fold
      [ HashMap.singleton name $ def &~ do
        pins .= HashMap.fromList (macroPins tech macroOptions)
        dims .= dimensions tech macroOptions
        vdd  .= fromMaybe def (macroVdd tech macroOptions)
        gnd  .= fromMaybe def (macroGnd tech macroOptions)
      | Macro name macroOptions _ <- macros
      ]
  stdCells <>= view stdCells tech


macroVdd :: Technology -> [MacroOption] -> Maybe Pin
macroVdd tech (MacroPin ident options _ : _)
  | MacroPinUse Power `elem` options
  = Just $ def & identifier .~ ident & dir .~ direction options & geometry .~ macroPorts tech options
macroVdd tech (_ : rest) = macroVdd tech rest
macroVdd _ _ = Nothing

macroGnd :: Technology -> [MacroOption] -> Maybe Pin
macroGnd tech (MacroPin ident options _ : _)
  | MacroPinUse Ground `elem` options
  = Just $ def & identifier .~ ident & dir .~ direction options & geometry .~ macroPorts tech options
macroGnd tech (_ : rest) = macroVdd tech rest
macroGnd _ _ = Nothing

macroPins :: Technology -> [MacroOption] -> [(Identifier, Pin)]
macroPins tech (MacroPin ident options _ : rest)
  | MacroPinUse Power `notElem` options
  , MacroPinUse Ground `notElem` options
  = (ident, def & identifier .~ ident & dir .~ direction options & geometry .~ macroPorts tech options)
  : macroPins tech rest
macroPins tech (_ : rest) = macroPins tech rest
macroPins _ [] = []


macroPorts tech (MacroPinPort (MacroPinPortLayer ident xs : _) : rest)
    = fmap (portLayerRectangle tech ident) xs ++ macroPorts tech rest
macroPorts tech (_ : rest) = macroPorts tech rest
macroPorts _ [] = []


portLayer "metal1" = Metal1
portLayer "metal2" = Metal2
portLayer "metal3" = Metal3
portLayer _ = AnyLayer


portLayerRectangle tech ident xs
    = constructPolygon (f $ round . (g *) <$> xs)
    & layers z .~ [portLayer ident] :: Polygon' LSC.Layer Int
    where g = view scaleFactor (tech :: Technology)
          f (x : y : ys) = (x, y) : f ys
          f _ = []


dimensions tech (MacroSize x y : _) = (round $ x * g, round $ y * g)
    where g = view scaleFactor (tech :: Technology)
dimensions tech (_ : rest) = dimensions tech rest
dimensions _ [] = (0, 0)


databaseUnits (Units (DatabaseList x) : _) = x
databaseUnits (_ : rest) = databaseUnits rest
databaseUnits [] = 1

direction (MacroPinDirection Input _ : _) = Just In
direction (MacroPinDirection Output _ : _) = Just Out
direction (MacroPinDirection InputOutput _ : _) = Just InOut
direction (_ : rest) = direction rest
direction [] = Nothing

