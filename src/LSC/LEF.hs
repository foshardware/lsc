{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}

module LSC.LEF where

import Control.Lens
import Control.Monad.State (get)
import Data.Map as Map

import LEF.Syntax
import LSC.Types


fromLEF :: LEF -> Bootstrap ()
fromLEF (LEF options _ _ _ _ macros) = do

  bootstrap $ set scaleFactor $ fromIntegral $ databaseUnits options

  tech <- get
  bootstrap $ set stdCells $ Map.fromList
    [ (name, Cell a b)
    | Macro name macroOptions _ <- macros
    , let a = Map.fromList $ macroPins tech macroOptions
    , let b = dims tech macroOptions
    ]

macroPins t (MacroPin ident options _ : rest) = (ident, Pin ident (direction options) (macroPort t options)) : macroPins t rest
macroPins t (_ : rest) = macroPins t rest
macroPins _ [] = []

macroPort t (MacroPinPort (MacroPinPortLayer ident : rest) : _) = Port (portLayer ident) (portRectangles t rest)
macroPort t (_ : rest) = macroPort t rest
macroPort _ [] = Port AnyLayer mempty

portLayer "metal1" = Metal1
portLayer "metal2" = Metal2
portLayer "metal3" = Metal3
portLayer _ = AnyLayer

portRectangles t (MacroPinPortRect a b c d : rest) = Rect
  (ceiling $ a * g, ceiling $ b * g)
  (ceiling $ c * g, ceiling $ d * g)
  : portRectangles t rest
  where g = scale t
portRectangles t (_ : rest) = portRectangles t rest
portRectangles _ [] = []

dims t (MacroSize x y : _) = (ceiling $ x * g, ceiling $ y * g)
  where g = scale t
dims t (_ : rest) = dims t rest
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
