
module LSC.Entropy where

import Data.Serialize.Get
import Data.Vector
import Data.Word
import Prelude hiding (replicate, sequence)

import System.Entropy


entropyVector32 :: Int -> IO (Vector Word32)
entropyVector32 n = do
    bytes <- getEntropy $ 4 * n
    either error pure $ sequence (replicate n getWord32be) `runGet` bytes

