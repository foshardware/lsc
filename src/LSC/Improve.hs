
module LSC.Improve where


import LSC.Types



improve :: Int -> (a -> LSC a) -> (a -> a -> Ordering) -> a -> LSC a
improve k action criterion x = do

    y <- improveStep k action criterion x

    case criterion x y of
        LT -> improve k action criterion y
        _  -> pure x



improveStep :: Int -> (a -> LSC a) -> (a -> a -> Ordering) -> a -> LSC a
improveStep k _ _ x | k <= 0 = pure x
improveStep k action criterion x = do

    y <- action x

    case criterion x y of
        LT -> improve k action criterion y
        _  -> improve (pred k) action criterion x

