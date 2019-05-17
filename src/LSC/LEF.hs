{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}

module LSC.LEF
  ( module Language.LEF.Parser
  , module Language.LEF.Syntax
  , fromLEF
  ) where

import Control.Lens
import Control.Monad.State (get)
import Data.Default
import Data.Map as Map

import Language.LEF.Parser (parseLEF)
import Language.LEF.Syntax

import LSC.Types


fromLEF :: LEF -> Bootstrap ()
fromLEF (LEF options _ _ _ _ macros) = do

  bootstrap $ set scaleFactor $ fromIntegral $ databaseUnits options

  tech <- get
  bootstrap $ set stdCells $ Map.fromList
    [ (,) name $ def &~ do
        pins .= Map.fromList (macroPins tech macroOptions)
        dims .= dimensions tech macroOptions
        vdd  .= maybe def id (macroVdd tech macroOptions)
        gnd  .= maybe def id (macroGnd tech macroOptions)
    | Macro name macroOptions _ <- macros
    ]

macroVdd :: Technology -> [MacroOption] -> Maybe Pin
macroVdd tech (MacroPin ident options _ : _)
  | MacroPinUse Power `elem` options
  = Just $ def & identifier .~ ident & dir .~ direction options & ports .~ macroPorts tech options
macroVdd tech (_ : rest) = macroVdd tech rest
macroVdd _ _ = Nothing

macroGnd :: Technology -> [MacroOption] -> Maybe Pin
macroGnd tech (MacroPin ident options _ : _)
  | MacroPinUse Ground `elem` options
  = Just $ def & identifier .~ ident & dir .~ direction options & ports .~ macroPorts tech options
macroGnd tech (_ : rest) = macroVdd tech rest
macroGnd _ _ = Nothing

macroPins :: Technology -> [MacroOption] -> [(Identifier, Pin)]
macroPins tech (MacroPin ident options _ : rest)
  | not $ MacroPinUse Power `elem` options
  , not $ MacroPinUse Ground `elem` options
  = (ident, def & identifier .~ ident & dir .~ direction options & ports .~ macroPorts tech options)
  : macroPins tech rest
macroPins tech (_ : rest) = macroPins tech rest
macroPins _ [] = []

macroPorts tech (MacroPinPort (MacroPinPortLayer ident : rest) : _) = portRectangles tech ident rest
macroPorts tech (_ : rest) = macroPorts tech rest
macroPorts _ [] = []

portLayer "metal1" = Metal1
portLayer "metal2" = Metal2
portLayer "metal3" = Metal3
portLayer _ = AnyLayer

portRectangles tech ident (MacroPinPortRect x1 y1 x2 y2 : rest) = Layered
  (ceiling $ x1 * g)
  (ceiling $ y1 * g)
  (ceiling $ x2 * g)
  (ceiling $ y2 * g)
  (pure $ portLayer ident)
  : portRectangles tech ident rest
  where g = scale tech
portRectangles tech ident (_ : rest) = portRectangles tech ident rest
portRectangles _ _ [] = []

dimensions tech (MacroSize x y : _) = (ceiling $ x * g, ceiling $ y * g)
  where g = scale tech
dimensions tech (_ : rest) = dimensions tech rest
dimensions _ [] = (0, 0)

scale :: Technology -> Double
scale = view scaleFactor

databaseUnits (Units (DatabaseList x) : _) = x
databaseUnits (_ : rest) = databaseUnits rest
databaseUnits [] = 1

direction (MacroPinDirection Input _ : _) = Just In
direction (MacroPinDirection Output _ : _) = Just Out
direction (MacroPinDirection InputOutput _ : _) = Just InOut
direction (_ : rest) = direction rest
direction [] = Nothing
