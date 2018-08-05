
module LSC.LEF where

import Data.Map as Map

import LEF.Syntax
import LSC.Types


fromLEF :: LEF -> Bootstrap ()
fromLEF (LEF options _ _ _ _ macros) = do

  bootstrap $ \ technology -> technology
    { scaleFactor = fromIntegral $ databaseUnits options
    }

  bootstrap $ \ technology ->

    let tech = technology
    in technology
    { components = Map.fromList
        [ (name, Component
            (Map.fromList $ pins tech macroOptions)
            (dims tech macroOptions))
        | Macro name macroOptions _ <- macros
        ]
    }

pins t (MacroPin ident options _ : rest) = (ident, Pin (direction options) (port t options)) : pins t rest
pins t (_ : rest) = pins t rest
pins _ [] = []

port t (MacroPinPort (MacroPinPortLayer ident : rest) : _) = Port ident (portRectangles t rest)
port t (_ : rest) = port t rest
port _ [] = Port mempty mempty

portRectangles t (MacroPinPortRect a b c d : rest) =
  ( ceiling $ a * scaleFactor t
  , ceiling $ b * scaleFactor t
  , ceiling $ c * scaleFactor t
  , ceiling $ d * scaleFactor t
  ) : portRectangles t rest
portRectangles t (_ : rest) = portRectangles t rest
portRectangles _ [] = []

dims t (MacroSize x y : _) = (ceiling $ x * scaleFactor t, ceiling $ y * scaleFactor t)
dims t (_ : rest) = dims t rest
dims _ [] = (0, 0)

databaseUnits (Units (DatabaseList x) : _) = x
databaseUnits (_ : rest) = databaseUnits rest
databaseUnits [] = 1

direction (MacroPinDirection Input _ : _) = In
direction (MacroPinDirection Output _ : _) = Out
direction (MacroPinDirection InputOutput _ : _) = InOut
direction (_ : rest) = direction rest
direction [] = InOut
