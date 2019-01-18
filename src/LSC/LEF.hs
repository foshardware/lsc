{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}

module LSC.LEF where

import Control.Lens
import Control.Monad.State (get)
import Data.Default
import Data.Map as Map

import LEF.Syntax
import LSC.Types


fromLEF :: LEF -> Bootstrap ()
fromLEF (LEF options _ _ _ _ macros) = do

  bootstrap $ set scaleFactor $ fromIntegral $ databaseUnits options

  tech <- get
  bootstrap $ set stdCells $ Map.fromList
    [ (name, def & pins .~ c & dimensions .~ d)
    | Macro name macroOptions _ <- macros
    , let c = Map.fromList $ macroPins tech macroOptions
    , let d = dims tech macroOptions
    ]

macroPins tech (MacroPin ident options _ : rest) = (ident, Pin ident (direction options) (macroPorts tech options)) : macroPins tech rest
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

dims tech (MacroSize x y : _) = (ceiling $ x * g, ceiling $ y * g)
  where g = scale tech
dims tech (_ : rest) = dims tech rest
dims _ [] = (0, 0)

scale :: Technology -> Double
scale = view scaleFactor

databaseUnits (Units (DatabaseList x) : _) = x
databaseUnits (_ : rest) = databaseUnits rest
databaseUnits [] = 1

direction (MacroPinDirection Input _ : _) = In
direction (MacroPinDirection Output _ : _) = Out
direction (MacroPinDirection InputOutput _ : _) = InOut
direction (_ : rest) = direction rest
direction [] = InOut
