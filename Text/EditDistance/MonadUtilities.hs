{-# LANGUAGE BangPatterns #-}

module Text.EditDistance.MonadUtilities where

{-# INLINE loopM_ #-}
loopM_ :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
loopM_ !from !to action
  | from > to = return ()
  | otherwise = do
    action from
    loopM_ (from + 1) to action