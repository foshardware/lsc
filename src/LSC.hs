{-# LANGUAGE TupleSections #-}

module LSC where

import Control.Arrow
import Control.Category
import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Codensity
import Control.Monad.Parallel
import Prelude hiding ((.), id)

import LSC.Placement
import LSC.Routing
import LSC.Types


stage1 :: Int -> Compiler NetGraph
stage1 _ = id
  >>> route &&& route
  >>> route *** route
  >>^ fst


route :: Compiler NetGraph
route = LS routeSat

place :: Compiler NetGraph
place = LS placeEasy


type Compiler a = LS a a

newtype LS a b = LS { compiler :: a -> LSC b }

instance Category LS where
  id = LS pure
  LS m . LS k = LS $ \ x -> do
    s <- thaw <$> ask
    liftIO $ runLSC s . m =<< runLSC s (k x)

instance Arrow LS where
  arr f = LS $ pure . f

  first (LS k) = LS $ \ (x, y) -> (, y) <$> k x

  LS k &&& LS m = LS $ \ x -> do
    s <- thaw <$> ask
    lift $ bindM2 (\ r1 r2 -> pure (r1, r2))
      (lowerCodensity $ liftIO $ runLSC s $ k x)
      (lowerCodensity $ liftIO $ runLSC s $ m x)

  LS k *** LS m = LS $ \ (x, y) -> do
    s <- thaw <$> ask
    lift $ bindM2 (\ r1 r2 -> pure (r1, r2))
      (lowerCodensity $ liftIO $ runLSC s $ k x)
      (lowerCodensity $ liftIO $ runLSC s $ m y)


instance ArrowZero LS where
  zeroArrow = throw $ AssertionFailed mempty

instance ArrowPlus LS where
  LS k <+> LS m = LS $ \ x -> do
    s <- thaw <$> ask
    liftIO $
      (runLSC s $ k x) `catch` \ (AssertionFailed _) ->
      (runLSC s $ m x)
