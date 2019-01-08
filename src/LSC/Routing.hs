{-# LANGUAGE TupleSections #-}

module LSC.Routing where

import Data.Foldable
import qualified Data.Map as Map
import Data.Vector (generate, fromList, (!))
import LSC.Types


routing :: NetGraph -> LSC NetGraph
routing netlist@(NetGraph name pins subs gates wires) = do

  newNetMapping <- Map.fromAscList
    <$> sequence [ (ident, ) <$> preliminary wire | (ident, wire) <- Map.toAscList wires ]

  pure $ NetGraph name pins subs gates newNetMapping


preliminary wire@(Net ident _ cs ix) = pure wire 

