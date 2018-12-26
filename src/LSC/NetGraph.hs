
module LSC.NetGraph where

import Control.Monad.Writer
import Data.Foldable
import Data.Semigroup
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.Internal.Debug
import Data.Text (Text, unpack)
import qualified Data.Vector as Vector

import LSC.Types


collectAtRoot :: NetGraph -> NetGraph
collectAtRoot netlist = netlist { subModels = execWriter $ new netlist }
  where
    new :: NetGraph -> Writer (Map Text NetGraph) ()
    new graph = do
      sequence_ $ new <$> subModels graph
      tell $ subModels graph


getLeaves :: NetGraph -> [NetGraph]
getLeaves netlist | null $ subModels netlist = [netlist]
getLeaves netlist = getLeaves =<< toList (subModels netlist)


showGraph :: NetGraph -> String
showGraph netlist = showTreeWith
  ( \ k x -> unwords [unpack k, showGraph x])
  True
  True
  (subModels netlist)

showNetHierarchy :: NetGraph -> String
showNetHierarchy netlist = unlines [ showNetHierarchy m | m <- toList $ subModels netlist ]
  ++ unlines
  [ mempty
  , "model: " ++ unpack (modelName netlist)
  , mempty
  , "Total: " ++ show (Vector.length $ gateVector netlist)
  ]
  ++ unlines [ unpack g ++ ": " ++ show c | (g, c) <- Map.assocs gates ]
  where
    gates = Map.fromListWith (+) [ (gateIdent g, 1 :: Int) | g <- toList $ gateVector netlist ]