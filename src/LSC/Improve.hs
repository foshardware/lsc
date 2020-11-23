
module LSC.Improve where


improve :: Monad m => Int -> (a -> a -> Int) -> a -> (a -> m a) -> m a
improve k criterion x action = do

    y <- improveStep k action criterion x
    -- y <- action x

    if criterion x y > 0
    then improve k criterion y action
    else pure x



improveStep :: Monad m => Int -> (a -> m a) -> (a -> a -> Int) -> a -> m a
improveStep k _ _ x | k <= 0 = pure x
improveStep k action criterion x = do

    y <- action x

    if criterion x y == 0
    then pure x
    else if criterion x y > 0
    then improve k criterion y action
    else improveStep (pred k) action criterion x


