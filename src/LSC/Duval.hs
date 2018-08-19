
module LSC.Duval where

import Control.Monad.Loops
import Control.Monad.ST
import Data.STRef
import Data.Vector
import Prelude hiding (length)

type String' a = Vector a

duval :: Ord a => String' a -> Vector (String' a)
duval s = runST $ do

  let n = length s
  i_ <- newSTRef 0
  factorization <- newSTRef mempty

  whileM_
      ( do
          i <- readSTRef i_
          pure $ i < n ) $ do

      j_ <- newSTRef . (+ 1) =<< readSTRef i_
      k_ <- newSTRef =<< readSTRef i_

      whileM_
          ( do
              j <- readSTRef j_
              k <- readSTRef k_
              pure $ j < n && s!k <= s!j ) $ do

          j <- readSTRef j_
          k <- readSTRef k_
          if s!k < s!j then
              writeSTRef k_ =<< readSTRef i_
          else
              modifySTRef k_ (+ 1)
      
          modifySTRef j_ (+ 1)

      whileM_
          ( do
              i <- readSTRef i_
              k <- readSTRef k_
              pure $ i <= k ) $ do
          i <- readSTRef i_
          j <- readSTRef j_
          k <- readSTRef k_
          modifySTRef factorization (`snoc` slice i (j - k) s)
          modifySTRef i_ (+ (j - k))

  readSTRef factorization

