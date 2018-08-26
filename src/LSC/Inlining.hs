
module LSC.Inlining where

import qualified Data.Map as Map
import Data.Foldable hiding (concat)
import Data.Monoid
import Data.Vector (Vector, concat, singleton, (!), generate)
import Prelude hiding (concat)

import LSC.Types


inlineAll :: Netlist -> Netlist
inlineAll (Netlist name pins subs nodes edges) = Netlist name

  pins

  mempty

  (foldr build nodes subs)

  edges

  where

    build sub ns = concat [ inline sub node | node <- toList ns ]


inline :: Netlist -> Gate -> Vector Gate
inline (Netlist name _ _ _ _) g
  | gateIdent g /= name
  = singleton g

inline (Netlist _ _ _ nodes _) g
  = generate (length nodes) 
  $ \ i -> (nodes ! i) { gateWires = rewire <$> gateWires (nodes ! i) }

    where

      assignments = Map.fromList $ gateWires g <> mapWires g
      rewire (k, v) = (k, maybe v id $ Map.lookup v assignments)




