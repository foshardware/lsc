
module LSC.Duval where

import Control.Monad.Loops
import Control.Monad.ST
import Data.STRef
import Data.Vector
import Data.Vector.Mutable (new, write)
import Prelude hiding (length)

type String' a = Vector a

duval :: Ord a => String' a -> Vector (String' a)
duval s = runST $ do

  let n = length s

  i_ <- newSTRef 0

  factorization <- new n
  c_ <- newSTRef 0

  whileM_ ((n >) <$> readSTRef i_) $ do

      j_ <- newSTRef . (+ 1) =<< readSTRef i_
      k_ <- newSTRef =<< readSTRef i_

      whileM_ (( \ j k -> j < n && s!k <= s!j ) <$> readSTRef j_ <*> readSTRef k_) $ do

          j <- readSTRef j_
          k <- readSTRef k_

          if s!k < s!j
          then writeSTRef k_ =<< readSTRef i_
          else modifySTRef k_ (+ 1)

          modifySTRef j_ (+ 1)

      whileM_ ((<=) <$> readSTRef i_ <*> readSTRef k_) $ do

          i <- readSTRef i_
          j <- readSTRef j_
          k <- readSTRef k_
          c <- readSTRef c_

          write factorization c (slice i (j - k) s)
          modifySTRef c_ (+ 1)

          modifySTRef i_ (+ (j - k))

  slice 0 <$> readSTRef c_ <*> unsafeFreeze factorization

