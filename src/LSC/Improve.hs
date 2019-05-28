
module LSC.Improve where

import Control.Lens

import LSC.Types



improve :: Monad m => Int -> (a -> a -> Ordering) -> a -> (a -> m a) -> m a
improve k criterion x action = do

    y <- improveStep k action criterion x

    case criterion x y of
        LT -> improve k criterion y action
        _  -> pure x



improveStep :: Monad m => Int -> (a -> m a) -> (a -> a -> Ordering) -> a -> m a
improveStep k _ _ x | k <= 0 = pure x
improveStep k action criterion x = do

    y <- action x

    case criterion x y of
        LT -> improve k criterion y action
        _  -> improveStep (pred k) action criterion x

