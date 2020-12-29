-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

module LSC.Entropy
  ( nonDeterministic
  , entropyVectorInt, entropyVector32
  , Permutation, randomPermutation
  , module System.Random.MWC
  ) where

import Control.Monad.Primitive
import Data.ByteString hiding (replicate)
import Data.Serialize.Get
import Data.Vector
import Data.Vector.Mutable (unsafeSwap)
import Data.Word
import Prelude hiding (replicate, sequence_)

import System.Entropy
import System.IO
import System.Random.MWC



nonDeterministic :: PrimBase m => Maybe Handle -> (Gen (PrimState m) -> m a) -> IO a
nonDeterministic Nothing action = do
    v <- entropyVector32 258
    unsafePrimToIO $ action =<< initialize v
nonDeterministic (Just handle) action = do
    seed <- hGet handle $ 258 * 4
    v <- either fail pure $ replicateM 258 getWord32be `runGet` seed
    unsafePrimToIO $ action =<< initialize v
{-# INLINABLE nonDeterministic #-}


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
    sequence_ $ generate (n - 1) $ \ i -> unsafeSwap v i =<< uniformR (i, n - 1) gen
    unsafeFreeze v
{-# INLINABLE randomPermutation #-}



entropyVector32 :: Int -> IO (Vector Word32)
entropyVector32 n = do
    seed <- getEntropy $ 4 * n
    either fail pure $ replicateM n getWord32be `runGet` seed


entropyVectorInt :: Int -> IO (Vector Int)
entropyVectorInt n = do
    seed <- getEntropy $ 8 * n
    either fail (pure . fmap fromIntegral) $ replicateM n getInt64be `runGet` seed

