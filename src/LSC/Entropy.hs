-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveAnyClass #-}

module LSC.Entropy
  ( nonDeterministically, deterministically
  , entropyVector32, entropyVector64
  , Permutation, randomPermutation
  , Gen, genBitSize
  ) where

import Control.Exception
import Control.Monad.Primitive
import Data.Bits
import Data.ByteString
import Data.Serialize.Get
import Data.Vector (Vector, generate, replicateM, unsafeFreeze, unsafeThaw)
import Data.Vector.Mutable (unsafeSwap)
import Data.Word

import System.Entropy
import System.IO
import qualified System.Random.MWC as MWC



size32 :: Int
size32 = 258


finiteOctetSize :: FiniteBits a => a -> Int
finiteOctetSize n = finiteBitSize n `shiftR` 3 


type Gen = MWC.Gen

genBitSize :: Int
genBitSize = size32 * finiteBitSize @Word32 0


newtype SourceRunOut = SourceRunOut String
  deriving Exception

instance Show SourceRunOut where
  show (SourceRunOut msg) = "entropy source: " ++ msg



nonDeterministically
  :: (PrimBase m, PrimState m ~ RealWorld)
  => Maybe Handle
  -> (Gen (PrimState m) -> m a)
  -> IO a
nonDeterministically Nothing action
  = do
    state <- entropyVector32 size32
    primToIO $ action =<< MWC.initialize state
nonDeterministically (Just h) action
  = do
    bytes <- hGet h $ size32 * finiteOctetSize @Word32 0
    state <- either (throwIO . SourceRunOut) pure $ replicateM size32 getWord32be `runGet` bytes
    primToIO $ action =<< MWC.initialize state
{-# INLINABLE nonDeterministically #-}


deterministically :: PrimBase m => (Gen (PrimState m) -> m a) -> m a
deterministically k = k =<< MWC.create
{-# INLINABLE deterministically #-}


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
    sequence_ $ generate (n - 1) $ \ i -> unsafeSwap v i =<< MWC.uniformR (i, n - 1) gen
    unsafeFreeze v
{-# INLINABLE randomPermutation #-}


entropyVector32 :: Int -> IO (Vector Word32)
entropyVector32 n = do
    seed <- getEntropy $ n * finiteOctetSize @Word32 0
    either fail pure $ replicateM n getWord32be `runGet` seed

entropyVector64 :: Int -> IO (Vector Word64)
entropyVector64 n = do
    seed <- getEntropy $ n * finiteOctetSize @Word32 0
    either fail pure $ replicateM n getWord64be `runGet` seed

