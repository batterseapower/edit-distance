module Text.EditDistance ( 
        EditCosts(..), defaultEditCosts, 
        levenshteinDistance, restrictedDamerauLevenshteinDistance
    ) where

import Text.EditDistance.EditCosts
import qualified Text.EditDistance.Bits as Bits
import qualified Text.EditDistance.STUArray as STUArray
import qualified Text.EditDistance.SquareSTUArray as SquareSTUArray

-- | Find the Levenshtein edit distance between two strings.  That is to say, the number of deletion,
-- insertion and substitution operations that are required to make the two strings equal.  Note that
-- this algorithm thereforec does not make use of the 'transpositionCost' field of the costs. See also:
-- <http://en.wikipedia.org/wiki/Levenshtein_distance>
levenshteinDistance :: EditCosts -> String -> String -> Int
levenshteinDistance costs 
  | costs == defaultEditCosts = Bits.levenshteinDistance
  | otherwise                 = STUArray.levenshteinDistance costs

-- | Find the "restricted" Damerau-Levenshtein edit distance between two strings.  This algorithm calculates the cost of
-- the so-called optimal string alignment, which does not always equal the appropriate edit distance. The cost of the optimal 
-- string alignment is the number of edit operations needed to make the input strings equal under the condition that no substring 
-- is edited more than once.  See also: <http://en.wikipedia.org/wiki/Damerau-Levenshtein_distance>.
restrictedDamerauLevenshteinDistance :: EditCosts -> String -> String -> Int
restrictedDamerauLevenshteinDistance costs 
  | costs == defaultEditCosts = Bits.restrictedDamerauLevenshteinDistance
  | otherwise                 = SquareSTUArray.restrictedDamerauLevenshteinDistance costs