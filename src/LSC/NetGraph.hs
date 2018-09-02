
module LSC.NetGraph where

import Data.Foldable

import LSC.Types


getLeaves :: NetGraph -> [NetGraph]
getLeaves netlist | null $ subModels netlist = [netlist]
getLeaves netlist = getLeaves =<< toList (subModels netlist)

