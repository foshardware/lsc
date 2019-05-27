
module LSC.Improve where

import Control.Lens

import LSC.Types



improve :: (a -> LSC a) -> (a -> a -> Ordering) -> a -> LSC a
improve action criterion x = do

    k <- view iterations <$> environment

    y <- improveStep k action criterion x

    case criterion x y of
        LT -> improve action criterion y
        _  -> pure x



improveStep :: Int -> (a -> LSC a) -> (a -> a -> Ordering) -> a -> LSC a
improveStep k _ _ x | k <= 0 = pure x
improveStep k action criterion x = do

    y <- action x

    case criterion x y of
        LT -> improve action criterion y
        _  -> improveStep (pred k) action criterion x

