{-# LANGUAGE RankNTypes #-}

module LSC.Operator where

import Data.SBV

(!) :: forall a b. SArray Integer b -> Integer -> SBV b
a ! k = readArray a $ literal k
infix 9 !