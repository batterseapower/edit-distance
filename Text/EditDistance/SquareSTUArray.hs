{-# LANGUAGE PatternGuards, ScopedTypeVariables, BangPatterns #-}

module Text.EditDistance.SquareSTUArray (
        levenshteinDistance, levenshteinDistanceWithLengths, restrictedDamerauLevenshteinDistance, restrictedDamerauLevenshteinDistanceWithLengths
    ) where

import Text.EditDistance.EditCosts
import Text.EditDistance.MonadUtilities

import Control.Monad
import Control.Monad.ST
import Data.Array.ST


levenshteinDistance :: EditCosts -> String -> String -> Int
levenshteinDistance !costs str1 str2 = levenshteinDistanceWithLengths costs str1_len str2_len str1 str2
  where
    str1_len = length str1
    str2_len = length str2

levenshteinDistanceWithLengths :: EditCosts -> Int -> Int -> String -> String -> Int
levenshteinDistanceWithLengths !costs !str1_len !str2_len str1 str2 = runST (levenshteinDistanceST costs str1_len str2_len str1 str2)

levenshteinDistanceST :: EditCosts -> Int -> Int -> String -> String -> ST s Int
levenshteinDistanceST !costs !str1_len !str2_len str1 str2 = do
    -- Create string arrays
    str1_array <- stringToArray str1 str1_len
    str2_array <- stringToArray str2 str2_len
    
    -- Create array of costs. Say we index it by (i, j) where i is the column index and j the row index.
    -- Rows correspond to characters of str2 and columns to characters of str1.
    cost_array <- newArray_ ((0, 0), (str1_len, str2_len)) :: ST s (STUArray s (Int, Int) Int)
    
     -- Fill out the first row (j = 0)
    _ <- (\f -> foldM f 0 ([1..] `zip` str1)) $ \deletion_cost (!i, col_char) -> let deletion_cost' = deletion_cost + deletionCost costs col_char in writeArray cost_array (i, 0) deletion_cost' >> return deletion_cost'
    
    -- Fill the remaining rows (j >= 1)
    _ <- (\f -> foldM f 0 [1..str2_len]) $ \insertion_cost (!j) -> do
        row_char <- readArray str2_array j
        
        -- Initialize the first element of the row (i = 0)
        let insertion_cost' = insertion_cost + insertionCost costs row_char
        writeArray cost_array (0, j) insertion_cost'
        
        -- Fill the remaining elements of the row (i >= 1)
        loopM_ 1 str1_len $ \(!i) -> do
            col_char <- readArray str1_array i
            
            cost <- standardCosts costs cost_array row_char col_char (i, j)
            writeArray cost_array (i, j) cost
        
        return insertion_cost'
    
    -- Return an actual answer
    readArray cost_array (str1_len, str2_len)


restrictedDamerauLevenshteinDistance :: EditCosts -> String -> String -> Int
restrictedDamerauLevenshteinDistance costs str1 str2 = restrictedDamerauLevenshteinDistanceWithLengths costs str1_len str2_len str1 str2
  where
    str1_len = length str1
    str2_len = length str2

restrictedDamerauLevenshteinDistanceWithLengths :: EditCosts -> Int -> Int -> String -> String -> Int
restrictedDamerauLevenshteinDistanceWithLengths costs str1_len str2_len str1 str2 = runST (restrictedDamerauLevenshteinDistanceST costs str1_len str2_len str1 str2)

restrictedDamerauLevenshteinDistanceST :: EditCosts -> Int -> Int -> String -> String -> ST s Int
restrictedDamerauLevenshteinDistanceST !costs str1_len str2_len str1 str2 = do
    -- Create string arrays
    str1_array <- stringToArray str1 str1_len
    str2_array <- stringToArray str2 str2_len
    
    -- Create array of costs. Say we index it by (i, j) where i is the column index and j the row index.
    -- Rows correspond to characters of str2 and columns to characters of str1.
    cost_array <- newArray_ ((0, 0), (str1_len, str2_len)) :: ST s (STUArray s (Int, Int) Int)
    
     -- Fill out the first row (j = 0)
    _ <- (\f -> foldM f 0 ([1..] `zip` str1)) $ \deletion_cost (!i, col_char) -> let deletion_cost' = deletion_cost + deletionCost costs col_char in writeArray cost_array (i, 0) deletion_cost' >> return deletion_cost'
    
    -- Fill out the second row (j = 1)
    when (str2_len > 0) $ do
        initial_row_char <- readArray str2_array 1
        
        -- Initialize the first element of the second row (i = 0)
        writeArray cost_array (0, 1) (insertionCost costs initial_row_char)
        
        -- Initialize the remaining elements of the row (i >= 1)
        loopM_ 1 str1_len $ \(!i) -> do
            col_char <- readArray str1_array i
            
            cost <- standardCosts costs cost_array initial_row_char col_char (i, 1)
            writeArray cost_array (i, 1) cost
    
    -- Fill the remaining rows (j >= 2)
    loopM_ 2 str2_len (\(!j) -> do
        row_char <- readArray str2_array j
        prev_row_char <- readArray str2_array (j - 1)
        
        -- Initialize the first element of the row (i = 0)
        writeArray cost_array (0, j) (insertionCost costs row_char * j)
        
        -- Initialize the second element of the row (i = 1)
        when (str1_len > 0) $ do
            col_char <- readArray str1_array 1
            
            cost <- standardCosts costs cost_array row_char col_char (1, j)
            writeArray cost_array (1, j) cost
        
        -- Fill the remaining elements of the row (i >= 2)
        loopM_ 2 str1_len (\(!i) -> do
            col_char <- readArray str1_array i
            prev_col_char <- readArray str1_array (i - 1)
            
            standard_cost <- standardCosts costs cost_array row_char col_char (i, j)
            cost <- if prev_row_char == col_char && prev_col_char == row_char
                    then do transpose_cost <- fmap (+ (transpositionCost costs col_char row_char)) $ readArray cost_array (i - 2, j - 2)
                            return (standard_cost `min` transpose_cost)
                    else return standard_cost
            writeArray cost_array (i, j) cost))
    
    -- Return an actual answer
    readArray cost_array (str1_len, str2_len)


{-# INLINE standardCosts #-}
standardCosts :: EditCosts -> STUArray s (Int, Int) Int -> Char -> Char -> (Int, Int) -> ST s Int
standardCosts !costs !cost_array !row_char !col_char (!i, !j) = do
    deletion_cost  <- fmap (+ (deletionCost costs col_char))  $ readArray cost_array (i - 1, j)
    insertion_cost <- fmap (+ (insertionCost costs row_char)) $ readArray cost_array (i, j - 1)
    subst_cost     <- fmap (+ if row_char == col_char
                                then 0 
                                else (substitutionCost costs col_char row_char))
                           (readArray cost_array (i - 1, j - 1))
    return $ deletion_cost `min` insertion_cost `min` subst_cost

{-# INLINE stringToArray #-}
stringToArray :: String -> Int -> ST s (STUArray s Int Char)
stringToArray str !str_len = do
    array <- newArray_ (1, str_len)
    forM_ (zip [1..] str) (uncurry (writeArray array))
    return array

{-
showArray :: STUArray s (Int, Int) Int -> ST s String
showArray array = do
    ((il, jl), (iu, ju)) <- getBounds array
    flip (flip foldM "") [(i, j) | i <- [il..iu], j <- [jl.. ju]] $ \rest (i, j) -> do
        elt <- readArray array (i, j)
        return $ rest ++ show (i, j) ++ ": " ++ show elt ++ ", "
-}
