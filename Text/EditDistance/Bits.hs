{-# LANGUAGE PatternGuards, ScopedTypeVariables, BangPatterns #-}

module Text.EditDistance.Bits (
        levenshteinDistance, restrictedDamerauLevenshteinDistance
    ) where

import Data.Bits
import Data.Char
import Data.List
import qualified Data.IntMap as IM


type BitVector = Integer


-- Based on the algorithm presented in "A Bit-Vector Algorithm for Computing Levenshtein and Damerau Edit Distances" in PSC'02 (Heikki Hyyro).
-- See http://www.cs.uta.fi/~helmu/pubs/psc02.pdf and http://www.cs.uta.fi/~helmu/pubs/PSCerr.html for an explanation
levenshteinDistance :: String -> String -> Int
levenshteinDistance str1 str2 
  | length str1 <= length str2 = levenshteinDistance' str1 str2
  | otherwise = levenshteinDistance' str2 str1

levenshteinDistance' :: String -> String -> Int
levenshteinDistance' str1 str2 
  | [] <- str1 = length str2
  | otherwise  = extractAnswer $ foldl' (levenshteinDistanceWorker (matchVectors str1) top_bit_mask vector_mask) (m_ones, 0, m) str2
  where m = length str1
        m_ones@vector_mask = (2 ^ m) - 1
        top_bit_mask = 1 `shiftL` (m - 1)
        extractAnswer (_, _, distance) = distance

levenshteinDistanceWorker :: IM.IntMap BitVector -> BitVector -> BitVector -> (BitVector, BitVector, Int) -> Char -> (BitVector, BitVector, Int)
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


-- Based on the algorithm presented in "A Bit-Vector Algorithm for Computing Levenshtein and Damerau Edit Distances" in PSC'02 (Heikki Hyyro).
-- See http://www.cs.uta.fi/~helmu/pubs/psc02.pdf and http://www.cs.uta.fi/~helmu/pubs/PSCerr.html for an explanation
restrictedDamerauLevenshteinDistance :: String -> String -> Int
restrictedDamerauLevenshteinDistance str1 str2
  | length str1 <= length str2 = restrictedDamerauLevenshteinDistance' str1 str2
  | otherwise = restrictedDamerauLevenshteinDistance' str2 str1

restrictedDamerauLevenshteinDistance' :: String -> String -> Int
restrictedDamerauLevenshteinDistance' str1 str2 
  | [] <- str1 = length str2
  | otherwise  = extractAnswer $ foldl' (restrictedDamerauLevenshteinDistanceWorker (matchVectors str1) top_bit_mask vector_mask) (0, 0, m_ones, 0, m) str2
  where m = length str1
        m_ones@vector_mask = (2 ^ m) - 1
        top_bit_mask = 1 `shiftL` (m - 1)
        extractAnswer (_, _, _, _, distance) = distance

restrictedDamerauLevenshteinDistanceWorker :: IM.IntMap BitVector -> BitVector -> BitVector -> (BitVector, BitVector, BitVector, BitVector, Int) -> Char -> (BitVector, BitVector, BitVector, BitVector, Int)
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


{-# INLINE sizedComplement #-}
sizedComplement :: BitVector -> BitVector -> BitVector
sizedComplement vector_mask vect = vector_mask `xor` vect

matchVectors :: String -> IM.IntMap BitVector
matchVectors = snd . foldl' go (0 :: Int, IM.empty)
  where
    go (!ix, !im) char = let ix' = ix + 1
                             im' = IM.insertWith (.|.) (ord char) (2 ^ ix) im
                         in (ix', im')