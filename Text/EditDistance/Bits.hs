{-# LANGUAGE PatternGuards, PatternSignatures, ScopedTypeVariables, BangPatterns #-}

module Text.EditDistance.Bits (
        levenshteinDistance, levenshteinDistanceWithLengths, {-levenshteinDistanceCutoff,-} restrictedDamerauLevenshteinDistance, restrictedDamerauLevenshteinDistanceWithLengths
    ) where

import Data.Bits
import Data.Char
import Data.Word
import Data.List
import qualified Data.IntMap as IM

--import Debug.Trace

--type BitVector = Integer

-- Based on the algorithm presented in "A Bit-Vector Algorithm for Computing Levenshtein and Damerau Edit Distances" in PSC'02 (Heikki Hyyro).
-- See http://www.cs.uta.fi/~helmu/pubs/psc02.pdf and http://www.cs.uta.fi/~helmu/pubs/PSCerr.html for an explanation
levenshteinDistance :: String -> String -> Int
levenshteinDistance str1 str2 = levenshteinDistanceWithLengths m n str1 str2
  where
    m = length str1
    n = length str2

levenshteinDistanceWithLengths :: Int -> Int -> String -> String -> Int
levenshteinDistanceWithLengths !m !n str1 str2
  | m <= n    = if n <= 32 -- n must be larger so this check is sufficient
                then levenshteinDistance' (undefined :: Word32) m n str1 str2
                else levenshteinDistance' (undefined :: Integer) m n str1 str2
  | otherwise = if m <= 32 -- m must be larger so this check is sufficient
                then levenshteinDistance' (undefined :: Word32) n m str2 str1
                else levenshteinDistance' (undefined :: Integer) n m str2 str1

{-# SPECIALIZE INLINE levenshteinDistance' :: Word32 -> Int -> Int -> String -> String -> Int #-}
{-# SPECIALIZE INLINE levenshteinDistance' :: Integer -> Int -> Int -> String -> String -> Int #-}
levenshteinDistance' :: (Num bv, Bits bv) => bv -> Int -> Int -> String -> String -> Int
levenshteinDistance' (_bv_dummy :: bv) !m !n str1 str2 
  | [] <- str1 = n
  | otherwise  = extractAnswer $ foldl' (levenshteinDistanceWorker (matchVectors str1) top_bit_mask vector_mask) (m_ones, 0, m) str2
  where m_ones@vector_mask = (2 ^ m) - 1
        top_bit_mask = 1 `shiftL` (m - 1) :: bv
        extractAnswer (_, _, distance) = distance

{-# SPECIALIZE levenshteinDistanceWorker :: IM.IntMap Word32 -> Word32 -> Word32 -> (Word32, Word32, Int) -> Char -> (Word32, Word32, Int) #-}
{-# SPECIALIZE levenshteinDistanceWorker :: IM.IntMap Integer -> Integer -> Integer -> (Integer, Integer, Int) -> Char -> (Integer, Integer, Int) #-}
levenshteinDistanceWorker :: (Num bv, Bits bv) => IM.IntMap bv -> bv -> bv -> (bv, bv, Int) -> Char -> (bv, bv, Int)
levenshteinDistanceWorker !str1_mvs !top_bit_mask !vector_mask (!vp, !vn, !distance) !char2 
  = {- trace (unlines ["pm = " ++ show pm'
                   ,"d0 = " ++ show d0'
                   ,"hp = " ++ show hp'
                   ,"hn = " ++ show hn'
                   ,"vp = " ++ show vp'
                   ,"vn = " ++ show vn'
                   ,"distance' = " ++ show distance'
                   ,"distance'' = " ++ show distance'']) -} (vp', vn', distance'')
  where
    pm' = IM.findWithDefault 0 (ord char2) str1_mvs
    
    d0' = ((((pm' .&. vp) + vp) .&. vector_mask) `xor` vp) .|. pm' .|. vn
    hp' = vn .|. sizedComplement vector_mask (d0' .|. vp)
    hn' = d0' .&. vp
    
    hp'_shift = ((hp' `shiftL` 1) .|. 1) .&. vector_mask
    hn'_shift = (hn' `shiftL` 1) .&. vector_mask
    vp' = hn'_shift .|. sizedComplement vector_mask (d0' .|. hp'_shift)
    vn' = d0' .&. hp'_shift
    
    distance' = if hp' .&. top_bit_mask /= 0 then distance + 1 else distance
    distance'' = if hn' .&. top_bit_mask /= 0 then distance' - 1 else distance'


{-

-- Just can't get this working!

-- Based on the algorithm presented in "A Bit-Vector Algorithm for Computing Levenshtein and Damerau Edit Distances" in PSC'02 (Heikki Hyyro).
-- See http://www.cs.uta.fi/~helmu/pubs/psc02.pdf and http://www.cs.uta.fi/~helmu/pubs/PSCerr.html for an explanation
levenshteinDistanceCutoff :: Int -> String -> String -> Int
levenshteinDistanceCutoff cutoff str1 str2 
  | length str1 <= length str2 = levenshteinDistanceCutoff' cutoff str1 str2
  | otherwise = levenshteinDistanceCutoff' cutoff str2 str1

levenshteinDistanceCutoff' :: Int -> String -> String -> Int
levenshteinDistanceCutoff' cutoff str1 str2 
  | [] <- str1 = n
  | otherwise  = extractAnswer $ foldl' (levenshteinDistanceCutoffFlatWorker (matchVectors str1))
                                    (foldl' (levenshteinDistanceCutoffDiagWorker (matchVectors str1)) (top_bit_mask, vector_mask, all_ones, 0, initial_pm_offset, initial_dist) str2_diag)
                                    str2_flat
  where m = length str1
        n = length str2
        vector_length = if testBit bottom_factor 0
                        then cutoff      -- Odd
                        else cutoff + 1  -- Even
        all_ones@vector_mask = (2 ^ vector_length) - 1
        top_bit_mask = trace (show bottom_factor ++ ", " ++ show vector_length) $ 1 `shiftL` (vector_length - 1)
        extractAnswer (_, _, _, _, _, distance) = distance
        
        len_difference = n - m
        top_factor = cutoff + len_difference
        bottom_factor = cutoff - len_difference
        bottom_factor_shift = (bottom_factor `shiftR` 1)
        
        initial_dist = bottom_factor_shift               -- The distance the virtual first vector ended on
        initial_pm_offset = (top_factor `shiftR` 1)      -- The amount of left shift to apply to the >next< pattern match vector
        diag_threshold = negate bottom_factor_shift + m  -- The index in str2 where we stop going diagonally down and start going across
        (str2_diag, str2_flat) = splitAt diag_threshold str2

levenshteinDistanceCutoffDiagWorker :: IM.IntMap BitVector -> (BitVector, BitVector, BitVector, BitVector, Int, Int) -> Char -> (BitVector, BitVector, BitVector, BitVector, Int, Int)
levenshteinDistanceCutoffDiagWorker !str1_mvs (!top_bit_mask, !vector_mask, !vp, !vn, !pm_offset, !distance) !char2 
  = trace (unlines ["vp = " ++ show vp
                   ,"vn = " ++ show vn
                   ,"vector_mask = " ++ show vector_mask
                   ,"pm_offset = " ++ show pm_offset
                   ,"unshifted_pm = " ++ show unshifted_pm
                   ,"pm' = " ++ show pm'
                   ,"d0' = " ++ show d0'
                   ,"hp' = " ++ show hp'
                   ,"hn' = " ++ show hn'
                   ,"vp' = " ++ show vp'
                   ,"vn' = " ++ show vn'
                   ,"distance' = " ++ show distance']) (top_bit_mask, vector_mask, vp', vn', pm_offset - 1, distance')
  where
    unshifted_pm = IM.findWithDefault 0 (ord char2) str1_mvs
    pm' = (unshifted_pm `shift` pm_offset) .&. vector_mask
    
    d0' = ((((pm' .&. vp) + vp) .&. vector_mask) `xor` vp) .|. pm' .|. vn
    hp' = vn .|. sizedComplement vector_mask (d0' .|. vp)
    hn' = d0' .&. vp
    
    d0'_shift = d0' `shiftR` 1
    vp' = hn' .|. sizedComplement vector_mask (d0'_shift .|. hp')
    vn' = d0'_shift .&. hp'
    
    distance' = if d0' .&. top_bit_mask /= 0 then distance else distance + 1

levenshteinDistanceCutoffFlatWorker :: IM.IntMap BitVector -> (BitVector, BitVector, BitVector, BitVector, Int, Int) -> Char -> (BitVector, BitVector, BitVector, BitVector, Int, Int)
levenshteinDistanceCutoffFlatWorker !str1_mvs (!top_bit_mask, !vector_mask, !vp, !vn, !pm_offset, !distance) !char2 
  = trace (unlines ["pm_offset = " ++ show pm_offset
                   ,"top_bit_mask' = " ++ show top_bit_mask'
                   ,"vector_mask' = " ++ show vector_mask'
                   ,"pm = " ++ show pm'
                   ,"d0 = " ++ show d0'
                   ,"hp = " ++ show hp'
                   ,"hn = " ++ show hn'
                   ,"vp = " ++ show vp'
                   ,"vn = " ++ show vn'
                   ,"distance' = " ++ show distance'
                   ,"distance'' = " ++ show distance'']) (top_bit_mask', vector_mask', vp', vn', pm_offset - 1, distance'')
  where
    top_bit_mask' = top_bit_mask `shiftR` 1
    vector_mask' = vector_mask `shiftR` 1
    pm' = (IM.findWithDefault 0 (ord char2) str1_mvs `rotate` pm_offset) .&. vector_mask'
    
    d0' = ((((pm' .&. vp) + vp) `xor` vp) .|. pm' .|. vn) .&. vector_mask'
    hp' = vn .|. sizedComplement vector_mask' (d0' .|. vp)
    hn' = d0' .&. vp
    
    d0'_shift = d0' `shiftR` 1
    vp' = hn' .|. sizedComplement vector_mask' (d0'_shift .|. hp')
    vn' = d0'_shift .&. hp'
    
    distance' = if hp' .&. top_bit_mask' /= 0 then distance + 1 else distance
    distance'' = if hn' .&. top_bit_mask' /= 0 then distance' - 1 else distance'

-}

-- Based on the algorithm presented in "A Bit-Vector Algorithm for Computing Levenshtein and Damerau Edit Distances" in PSC'02 (Heikki Hyyro).
-- See http://www.cs.uta.fi/~helmu/pubs/psc02.pdf and http://www.cs.uta.fi/~helmu/pubs/PSCerr.html for an explanation
restrictedDamerauLevenshteinDistance :: String -> String -> Int
restrictedDamerauLevenshteinDistance str1 str2 = restrictedDamerauLevenshteinDistanceWithLengths m n str1 str2
  where
    m = length str1
    n = length str2

restrictedDamerauLevenshteinDistanceWithLengths :: Int -> Int -> String -> String -> Int
restrictedDamerauLevenshteinDistanceWithLengths !m !n str1 str2
  | m <= n    = if n <= 32 -- n must be larger so this check is sufficient
                then restrictedDamerauLevenshteinDistance' (undefined :: Word32) m n str1 str2
                else restrictedDamerauLevenshteinDistance' (undefined :: Integer) m n str1 str2
  | otherwise = if m <= 32 -- m must be larger so this check is sufficient
                then restrictedDamerauLevenshteinDistance' (undefined :: Word32) n m str2 str1
                else restrictedDamerauLevenshteinDistance' (undefined :: Integer) n m str2 str1

{-# SPECIALIZE INLINE restrictedDamerauLevenshteinDistance' :: Word32 -> Int -> Int -> String -> String -> Int #-}
{-# SPECIALIZE INLINE restrictedDamerauLevenshteinDistance' :: Integer -> Int -> Int -> String -> String -> Int #-}
restrictedDamerauLevenshteinDistance' :: (Num bv, Bits bv) => bv -> Int -> Int -> String -> String -> Int
restrictedDamerauLevenshteinDistance' (_bv_dummy :: bv) !m !n str1 str2 
  | [] <- str1 = n
  | otherwise  = extractAnswer $ foldl' (restrictedDamerauLevenshteinDistanceWorker (matchVectors str1) top_bit_mask vector_mask) (0, 0, m_ones, 0, m) str2
  where m_ones@vector_mask = (2 ^ m) - 1
        top_bit_mask = 1 `shiftL` (m - 1) :: bv
        extractAnswer (_, _, _, _, distance) = distance

{-# SPECIALIZE restrictedDamerauLevenshteinDistanceWorker :: IM.IntMap Word32 -> Word32 -> Word32 -> (Word32, Word32, Word32, Word32, Int) -> Char -> (Word32, Word32, Word32, Word32, Int) #-}
{-# SPECIALIZE restrictedDamerauLevenshteinDistanceWorker :: IM.IntMap Integer -> Integer -> Integer -> (Integer, Integer, Integer, Integer, Int) -> Char -> (Integer, Integer, Integer, Integer, Int) #-}
restrictedDamerauLevenshteinDistanceWorker :: (Num bv, Bits bv) => IM.IntMap bv -> bv -> bv -> (bv, bv, bv, bv, Int) -> Char -> (bv, bv, bv, bv, Int)
restrictedDamerauLevenshteinDistanceWorker !str1_mvs !top_bit_mask !vector_mask (!pm, !d0, !vp, !vn, !distance) !char2 
  = (pm', d0', vp', vn', distance'')
  where
    pm' = IM.findWithDefault 0 (ord char2) str1_mvs
    
    d0' = ((((sizedComplement vector_mask d0) .&. pm') `shiftL` 1) .&. pm) -- No need to mask the shiftL because of the restricted range of pm
      .|. ((((pm' .&. vp) + vp) .&. vector_mask) `xor` vp) .|. pm' .|. vn
    hp' = vn .|. sizedComplement vector_mask (d0' .|. vp)
    hn' = d0' .&. vp
    
    hp'_shift = ((hp' `shiftL` 1) .|. 1) .&. vector_mask
    hn'_shift = (hn' `shiftL` 1) .&. vector_mask
    vp' = hn'_shift .|. sizedComplement vector_mask (d0' .|. hp'_shift)
    vn' = d0' .&. hp'_shift
    
    distance' = if hp' .&. top_bit_mask /= 0 then distance + 1 else distance
    distance'' = if hn' .&. top_bit_mask /= 0 then distance' - 1 else distance'


{-# SPECIALIZE INLINE sizedComplement :: Word32 -> Word32 -> Word32 #-}
{-# SPECIALIZE INLINE sizedComplement :: Integer -> Integer -> Integer #-}
sizedComplement :: (Num bv, Bits bv) => bv -> bv -> bv
sizedComplement vector_mask vect = vector_mask `xor` vect

{-# SPECIALIZE matchVectors :: String -> IM.IntMap Word32 #-}
{-# SPECIALIZE matchVectors :: String -> IM.IntMap Integer #-}
matchVectors :: (Num bv, Bits bv) => String -> IM.IntMap bv
matchVectors = snd . foldl' go (0 :: Int, IM.empty)
  where
    go (!ix, !im) char = let ix' = ix + 1
                             im' = IM.insertWith (.|.) (ord char) (2 ^ ix) im
                         in (ix', im')