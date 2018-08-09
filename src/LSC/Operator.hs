{-# LANGUAGE RankNTypes #-}

module LSC.Operator where

import Data.SBV

(!) :: forall a. SArray Integer a -> Integer -> SBV a
a ! k = readArray a $ literal k
infix 9 !
