
module LSC.FM where

import Control.Lens
import Control.Monad.ST
import Data.Monoid
import Data.IntSet (IntSet)
import qualified Data.IntSet as Set
import Data.IntMap (IntMap)
import qualified Data.IntMap as Map
import Data.Tuple
import Data.Vector hiding (modify)
import Data.Vector.Mutable hiding (swap, replicate)
import Prelude hiding (replicate)


type N = Int -- Net
type C = Int -- Cell

type NetArray  = Vector IntSet
type CellArray = Vector IntSet


initialGain :: Vector Int
initialGain = undefined


inputRoutine :: (Foldable f, Functor f) => N -> C -> f (N, C) -> (CellArray, NetArray)
inputRoutine n c xs = (cellArray, netArray)
  where
    cellArray = ala Endo foldMap (unsafeArray        <$> xs) (replicate c mempty)
    netArray  = ala Endo foldMap (unsafeArray . swap <$> xs) (replicate n mempty)


unsafeArray :: (Int, Int) -> Vector IntSet -> Vector IntSet
unsafeArray (x, y) v = runST $ do
  m <- unsafeThaw v
  modify m (Set.insert x) y
  unsafeFreeze m

