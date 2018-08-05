
module LSC.LEF where

import Data.Map as Map

import LEF.Syntax
import LSC.Types


fromLEF :: LEF -> Bootstrap ()
fromLEF (LEF options _ _ _ _ macros) = bootstrap $ \ technology ->

  let units = fromIntegral $ databaseUnits options

  in technology

  { components = Map.fromList
      [ (name, Component
          (Map.fromList $ pins macroOptions)
          (let (x, y) = dims macroOptions in (ceiling $ x * units, ceiling $ y * units)))
      | Macro name macroOptions _ <- macros
      ]
  }

pins (MacroPin ident options _ : rest) = (ident, Pin
  (direction options)
  (contacts options)
  (layer options))
  : pins rest
pins (_ : rest) = pins rest
pins [] = []

dims (MacroSize x y : _) = (x, y)
dims (_ : rest) = dims rest
dims [] = (0, 0)

databaseUnits (Units (DatabaseList x) : _) = x
databaseUnits (_ : rest) = databaseUnits rest
databaseUnits [] = 1

direction = undefined
contacts = undefined
layer = undefined
