
module LSC.Symbolic
  ( module Data.SBV
  , module Data.SBV.Control
  , MonadSymbolic(..)
  ) where

import Control.Monad.Codensity
import Control.Monad.Trans

import Data.SBV hiding (MonadSymbolic)
import Data.SBV.Control


class Monad m => MonadSymbolic m where
  liftSymbolic :: Symbolic a -> m a

instance MonadSymbolic m => MonadSymbolic (Codensity m) where
  liftSymbolic = lift . liftSymbolic

