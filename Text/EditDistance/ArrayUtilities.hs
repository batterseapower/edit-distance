{-# LANGUAGE CPP #-}

module Text.EditDistance.ArrayUtilities (
    unsafeReadArray, unsafeWriteArray,
    unsafeReadArray', unsafeWriteArray',
    stringToArray
  ) where

import Control.Monad (forM_)
import Control.Monad.ST

import Data.Array.ST
import Data.Array.Base (unsafeRead, unsafeWrite, getNumElements)

#ifdef __GLASGOW_HASKELL__
import GHC.Arr (unsafeIndex)
#else
import Data.Ix (index)

{-# INLINE unsafeIndex #-}
unsafeIndex :: Ix i => (i, i) -> i -> Int
unsafeIndex = index
#endif


{-# INLINE unsafeReadArray #-}
unsafeReadArray :: (MArray a e m, Ix i) => a i e -> i -> m e
unsafeReadArray marr i = do
    f <- unsafeReadArray' marr
    f i

{-# INLINE unsafeWriteArray #-}
unsafeWriteArray :: (MArray a e m, Ix i) => a i e -> i -> e -> m ()
unsafeWriteArray marr i e = do
  f <- unsafeWriteArray' marr
  f i e


{-# INLINE unsafeReadArray' #-}
unsafeReadArray' :: (MArray a e m, Ix i) => a i e -> m (i -> m e)
unsafeReadArray' marr = do
    (l,u) <- getBounds marr
    return $ \i -> unsafeRead marr (unsafeIndex (l,u) i)

{-# INLINE unsafeWriteArray' #-}
unsafeWriteArray' :: (MArray a e m, Ix i) => a i e -> m (i -> e -> m ())
unsafeWriteArray' marr = do
  (l,u) <- getBounds marr
  return $ \i e -> unsafeWrite marr (unsafeIndex (l,u) i) e

{-# INLINE stringToArray #-}
stringToArray :: String -> Int -> ST s (STUArray s Int Char)
stringToArray str str_len = do
    array <- newArray_ (1, str_len)
    write <- unsafeWriteArray' array
    forM_ (zip [1..] str) (uncurry write)
    return array

{-
showArray :: STUArray s (Int, Int) Int -> ST s String
showArray array = do
    ((il, jl), (iu, ju)) <- getBounds array
    flip (flip foldM "") [(i, j) | i <- [il..iu], j <- [jl.. ju]] $ \rest (i, j) -> do
        elt <- readArray array (i, j)
        return $ rest ++ show (i, j) ++ ": " ++ show elt ++ ", "
-}
