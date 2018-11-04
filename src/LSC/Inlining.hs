
module LSC.Inlining where

import Control.Applicative
import qualified Data.Map as Map
import Data.Foldable hiding (concat)
import Data.Monoid
import Data.Vector (Vector, concat, singleton, (!), generate)
import Prelude hiding (concat)

import LSC.Types


inlineAll :: NetGraph -> NetGraph
inlineAll (NetGraph name pins subs nodes edges) = NetGraph name

  pins

  mempty

  (foldr build nodes subs)

  edges

  where

    build sub ns = concat [ inline sub node | node <- toList ns ]


inline :: NetGraph -> Gate -> Vector Gate
inline (NetGraph name _ _ _ _) g
  | gateIdent g /= name
  = singleton g

inline (NetGraph _ _ _ nodes _) g
  = generate (length nodes) 
  $ \ i -> (nodes ! i) { gateWires = rewire <$> gateWires (nodes ! i) }

    where

      rewire v = maybe v id $ Map.lookup v (gateWires g) <|> Map.lookup v (mapWires g)




