{-# LANGUAGE TupleSections #-}

module LSC where

import Control.Arrow
import Control.Category
import Control.Exception
import Control.Monad.IO.Class
import Prelude hiding ((.))

import LSC.Placement
import LSC.Routing
import LSC.Types


stage1 :: Int -> Compiler NetGraph
stage1 _
    = route
  <+> route


route :: Compiler NetGraph
route = LS routeSat

place :: Compiler NetGraph
place = LS placeEasy


compiler :: Compiler a -> a -> LSC a
compiler (LS a) = a

type Compiler a = LS a a

newtype LS a b = LS (a -> LSC b)

instance Category LS where
  LS b . LS a = LS $ \ x -> do
    t <- thaw <$> ask
    liftIO $ runLSC t . b =<< runLSC t (a x)

instance Arrow LS where
  arr f = LS $ pure . f
  first (LS a) = LS $ \ (b, d) -> (, d) <$> a b

instance ArrowZero LS where
  zeroArrow = throw $ AssertionFailed mempty

instance ArrowPlus LS where
  LS a <+> LS b = LS $ \ x -> do
    t <- thaw <$> ask
    liftIO $
      (runLSC t $ a x) `catch` \ (AssertionFailed _) ->
      (runLSC t $ b x)
