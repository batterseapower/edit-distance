{-# LANGUAGE PatternGuards #-}

-- | Computing the edit distances between strings
module Text.EditDistance ( 
        Costs(..), EditCosts(..), defaultEditCosts, 
        levenshteinDistance, restrictedDamerauLevenshteinDistance
    ) where

import Text.EditDistance.EditCosts
import qualified Text.EditDistance.Bits as Bits
import qualified Text.EditDistance.SquareSTUArray as SquareSTUArray

-- | Find the Levenshtein edit distance between two strings.  That is to say, the number of deletion,
-- insertion and substitution operations that are required to make the two strings equal.  Note that
-- this algorithm therefore does not make use of the 'transpositionCost' field of the costs. See also:
-- <http://en.wikipedia.org/wiki/Levenshtein_distance>.
levenshteinDistance :: EditCosts -> String -> String -> Int
levenshteinDistance costs str1 str2
  | isDefaultEditCosts costs
  , (str1_len <= 64) == (str2_len <= 64)                             -- The Integer implementation of the Bits algorithm is quite inefficient, but scales better than the
  = Bits.levenshteinDistanceWithLengths str1_len str2_len str1 str2  -- STUArrays if both string lengths > 64. The Word64 implementation is always better, if it is applicable
  | otherwise
  = SquareSTUArray.levenshteinDistanceWithLengths costs str1_len str2_len str1 str2 -- SquareSTUArray usually beat making more use of the heap with STUArray
  where
    str1_len = length str1
    str2_len = length str2

-- | Find the \"restricted\" Damerau-Levenshtein edit distance between two strings.  This algorithm calculates the cost of
-- the so-called optimal string alignment, which does not always equal the appropriate edit distance. The cost of the optimal 
-- string alignment is the number of edit operations needed to make the input strings equal under the condition that no substring 
-- is edited more than once.  See also: <http://en.wikipedia.org/wiki/Damerau-Levenshtein_distance>.
restrictedDamerauLevenshteinDistance :: EditCosts -> String -> String -> Int
restrictedDamerauLevenshteinDistance costs str1 str2
  | isDefaultEditCosts costs
  , (str1_len <= 64) == (str2_len <= 64)                                             -- The Integer implementation of the Bits algorithm is quite inefficient, but scales better than the
  = Bits.restrictedDamerauLevenshteinDistanceWithLengths str1_len str2_len str1 str2 -- STUArrays if both string lengths > 64. The Word64 implementation is always better, if it is applicable
  | otherwise
  = SquareSTUArray.restrictedDamerauLevenshteinDistanceWithLengths costs str1_len str2_len str1 str2 -- SquareSTUArray usually beat making more use of the heap with STUArray
  where
    str1_len = length str1
    str2_len = length str2
