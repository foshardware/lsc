-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

module LSC.Entropy
  ( nonDeterministic
  , entropyVectorInt, entropyVector32
  , Permutation, randomPermutation
  , module System.Random.MWC
  ) where

import Control.Monad.Primitive
import Data.Foldable
import Data.Serialize.Get
import Data.Vector
import Data.Vector.Mutable (unsafeSwap)
import Data.Word
import Prelude hiding (replicate, sequence)

import System.Entropy
import System.Random.MWC



nonDeterministic :: PrimBase m => (Gen (PrimState m) -> m a) -> IO a
nonDeterministic action = do
    v <- entropyVector32 258
    unsafePrimToIO $ action =<< initialize v


type Permutation = Vector Int

-- | This function does not reach all possible permutations for lists
--   consisting of more than 969 elements. Any PRNGs possible states
--   are bound by its possible seed values.
--   In the case of MWC8222 the period is 2^8222 which allows for
--   not more than 969! different states.
--
-- seed bits: 8222
-- maximum list length: 969
--
--   969! =~ 2^8222
--
-- Monotonicity of  n! / (2^n):
--
-- desired seed bits: 256909
-- desired list length: 20000
--
--   20000! =~ 2^256909
--
randomPermutation :: PrimBase m => Int -> Gen (PrimState m) -> m Permutation
randomPermutation n gen = do
  v <- unsafeThaw $ generate n id
  for_ [0 .. n - 2] $ \ i -> unsafeSwap v i =<< uniformR (i, n - 1) gen
  unsafeFreeze v



entropyVector32 :: Int -> IO (Vector Word32)
entropyVector32 n = do
    bytes <- getEntropy $ 4 * n
    either error pure $ sequence (replicate n getWord32be) `runGet` bytes


entropyVectorInt :: Int -> IO (Vector Int)
entropyVectorInt n = do
    bytes <- getEntropy $ 8 * n
    either error pure $ sequence (replicate n $ fromIntegral <$> getInt64be) `runGet` bytes

