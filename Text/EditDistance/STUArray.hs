{-# LANGUAGE PatternGuards, ScopedTypeVariables, BangPatterns #-}

module Text.EditDistance.STUArray (
        levenshteinDistance, levenshteinDistanceWithLengths, restrictedDamerauLevenshteinDistance, restrictedDamerauLevenshteinDistanceWithLengths
    ) where

import Text.EditDistance.EditCosts
import Text.EditDistance.MonadUtilities
import Text.EditDistance.ArrayUtilities

import Control.Monad hiding (foldM)
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
    
    -- Create array of costs for a single row. Say we index costs by (i, j) where i is the column index and j the row index.
    -- Rows correspond to characters of str2 and columns to characters of str1. We can get away with just storing a single
    -- row of costs at a time, but we use two because it turns out to be faster
    cost_row  <- newArray_ (0, str1_len) :: ST s (STUArray s Int Int)
    cost_row' <- newArray_ (0, str1_len) :: ST s (STUArray s Int Int)

    read_str1 <- unsafeReadArray' str1_array
    read_str2 <- unsafeReadArray' str2_array
    
     -- Fill out the first row (j = 0)
    _ <- (\f -> foldM f (1, 0) str1) $ \(i, deletion_cost) col_char -> let deletion_cost' = deletion_cost + deletionCost costs col_char in unsafeWriteArray cost_row i deletion_cost' >> return (i + 1, deletion_cost')
    
    -- Fill out the remaining rows (j >= 1)
    (_, final_row, _) <- (\f -> foldM f (0, cost_row, cost_row') [1..str2_len]) $ \(!insertion_cost, !cost_row, !cost_row') !j -> do
        row_char <- read_str2 j
        
        -- Initialize the first element of the row (i = 0)
        let insertion_cost' = insertion_cost + insertionCost costs row_char
        unsafeWriteArray cost_row' 0 insertion_cost'
        
        -- Fill the remaining elements of the row (i >= 1)
        loopM_ 1 str1_len $ \(!i) -> do
            col_char <- read_str1 i
            
            left_up <- unsafeReadArray cost_row  (i - 1)
            left    <- unsafeReadArray cost_row' (i - 1)
            here_up <- unsafeReadArray cost_row i
            let here = standardCosts costs row_char col_char left left_up here_up
            unsafeWriteArray cost_row' i here

        return (insertion_cost', cost_row', cost_row)


    -- Return an actual answer
    unsafeReadArray final_row str1_len

restrictedDamerauLevenshteinDistance :: EditCosts -> String -> String -> Int
restrictedDamerauLevenshteinDistance !costs str1 str2 = restrictedDamerauLevenshteinDistanceWithLengths costs str1_len str2_len str1 str2
  where
    str1_len = length str1
    str2_len = length str2

restrictedDamerauLevenshteinDistanceWithLengths :: EditCosts -> Int -> Int -> String -> String -> Int
restrictedDamerauLevenshteinDistanceWithLengths !costs !str1_len !str2_len str1 str2 = runST (restrictedDamerauLevenshteinDistanceST costs str1_len str2_len str1 str2)

restrictedDamerauLevenshteinDistanceST :: EditCosts -> Int -> Int -> String -> String -> ST s Int
restrictedDamerauLevenshteinDistanceST !costs str1_len str2_len str1 str2 = do
    -- Create string arrays
    str1_array <- stringToArray str1 str1_len
    str2_array <- stringToArray str2 str2_len
    
    -- Create array of costs for a single row. Say we index costs by (i, j) where i is the column index and j the row index.
    -- Rows correspond to characters of str2 and columns to characters of str1. We can get away with just storing two
    -- rows of costs at a time, but I use three because it turns out to be faster
    cost_row <- newArray_ (0, str1_len) :: ST s (STUArray s Int Int)

    read_str1 <- unsafeReadArray' str1_array
    read_str2 <- unsafeReadArray' str2_array
    
    -- Fill out the first row (j = 0)
    _ <- (\f -> foldM f (1, 0) str1) $ \(i, deletion_cost) col_char -> let deletion_cost' = deletion_cost + deletionCost costs col_char in unsafeWriteArray cost_row i deletion_cost' >> return (i + 1, deletion_cost')
    
    if (str2_len == 0)
      then unsafeReadArray cost_row str1_len
      else do
        -- We defer allocation of these arrays to here because they aren't used in the other branch
        cost_row'  <- newArray_ (0, str1_len) :: ST s (STUArray s Int Int)
        cost_row'' <- newArray_ (0, str1_len) :: ST s (STUArray s Int Int)
        
        -- Fill out the second row (j = 1)
        row_char <- read_str2 1

        -- Initialize the first element of the row (i = 0)
        let zero = insertionCost costs row_char
        unsafeWriteArray cost_row' 0 zero

        -- Fill the remaining elements of the row (i >= 1)
        loopM_ 1 str1_len (firstRowColWorker read_str1 row_char cost_row cost_row')
        
        -- Fill out the remaining rows (j >= 2)
        (_, _, final_row, _, _) <- foldM (restrictedDamerauLevenshteinDistanceSTRowWorker costs str1_len read_str1 read_str2) (zero, cost_row, cost_row', cost_row'', row_char) [2..str2_len]
        
        -- Return an actual answer
        unsafeReadArray final_row str1_len
  where    
    {-# INLINE firstRowColWorker #-}
    firstRowColWorker read_str1 !row_char !cost_row !cost_row' !i = do
        col_char <- read_str1 i
        
        left_up <- unsafeReadArray cost_row  (i - 1)
        left    <- unsafeReadArray cost_row' (i - 1)
        here_up <- unsafeReadArray cost_row  i
        let here = standardCosts costs row_char col_char left left_up here_up
        unsafeWriteArray cost_row' i here

{-# INLINE restrictedDamerauLevenshteinDistanceSTRowWorker #-}
restrictedDamerauLevenshteinDistanceSTRowWorker :: EditCosts -> Int
                                                -> (Int -> ST s Char) -> (Int -> ST s Char) -- String array accessors
                                                -> (Int, STUArray s Int Int, STUArray s Int Int, STUArray s Int Int, Char) -> Int -- Incoming rows of the matrix in recency order
                                                -> ST s (Int, STUArray s Int Int, STUArray s Int Int, STUArray s Int Int, Char)   -- Outgoing rows of the matrix in recency order
restrictedDamerauLevenshteinDistanceSTRowWorker !costs !str1_len read_str1 read_str2 (!insertion_cost, !cost_row, !cost_row', !cost_row'', !prev_row_char) !j = do
    row_char <- read_str2 j
    
    -- Initialize the first element of the row (i = 0)
    zero_up    <- unsafeReadArray cost_row' 0
    let insertion_cost' = insertion_cost + insertionCost costs row_char
    unsafeWriteArray cost_row'' 0 insertion_cost'
    
    -- Initialize the second element of the row (i = 1)
    when (str1_len > 0) $ do
        col_char <- read_str1 1
        one_up   <- unsafeReadArray cost_row' 1
        let one = standardCosts costs row_char col_char insertion_cost' zero_up one_up
        unsafeWriteArray cost_row'' 1 one
        
        -- Fill the remaining elements of the row (i >= 2)
        loopM_ 2 str1_len (colWorker row_char)
    
    return (insertion_cost', cost_row', cost_row'', cost_row, row_char)
  where
    colWorker !row_char !i = do
        prev_col_char <- read_str1 (i - 1)
        col_char <- read_str1 i
        
        left_left_up_up <- unsafeReadArray cost_row (i - 2)
        left_up    <- unsafeReadArray cost_row'  (i - 1)
        left       <- unsafeReadArray cost_row'' (i - 1)
        here_up    <- unsafeReadArray cost_row' i
        let here_standard_only = standardCosts costs row_char col_char left left_up here_up
            here = if prev_row_char == col_char && prev_col_char == row_char
                   then here_standard_only `min` (left_left_up_up + transpositionCost costs col_char row_char)
                   else here_standard_only
        
        unsafeWriteArray cost_row'' i here


{-# INLINE standardCosts #-}
standardCosts :: EditCosts -> Char -> Char -> Int -> Int -> Int -> Int
standardCosts !costs !row_char !col_char !cost_left !cost_left_up !cost_up = deletion_cost `min` insertion_cost `min` subst_cost
  where
    deletion_cost  = cost_left + deletionCost costs col_char
    insertion_cost = cost_up + insertionCost costs row_char
    subst_cost     = cost_left_up + if row_char == col_char then 0 else substitutionCost costs col_char row_char
