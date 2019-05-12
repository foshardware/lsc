
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

entropyVectorInt :: Int -> IO (Vector Int)
entropyVectorInt n = do
    bytes <- getEntropy $ 8 * n
    either error pure $ sequence (replicate n $ fromIntegral <$> getInt64be) `runGet` bytes

