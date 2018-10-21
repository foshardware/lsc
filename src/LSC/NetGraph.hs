
module LSC.NetGraph where

import Data.Foldable
import Data.Map.Internal.Debug
import Data.Text (unpack)

import LSC.Types


getLeaves :: NetGraph -> [NetGraph]
getLeaves netlist | null $ subModels netlist = [netlist]
getLeaves netlist = getLeaves =<< toList (subModels netlist)


showGraph :: NetGraph -> String
showGraph netlist = showTreeWith
  ( \ k x -> unwords [unpack k, showGraph x])
  True
  True
  (subModels netlist)

