{-# LANGUAGE BangPatterns #-}

module Text.EditDistance.MonadUtilities where

{-# INLINE loopM_ #-}
loopM_ :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
loopM_ from to action = go from to
  where
    go from to | from > to = return ()
               | otherwise = do action from
                                go (from + 1) to

-- foldM in Control.Monad is not defined using SAT style so optimises very poorly
{-# INLINE foldM #-}
foldM             :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldM f a xs = foldr (\x rest a -> f a x >>= rest) return xs a
{-
-- If we define it like this, then we aren't able to deforest wrt. a "build" in xs, which would be sad :(
foldM f = go
  where go a (x:xs)  =  f a x >>= \fax -> go fax xs
        go a []      =  return a
-}
