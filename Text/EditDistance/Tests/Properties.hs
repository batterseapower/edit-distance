{-# LANGUAGE PatternGuards, PatternSignatures, ScopedTypeVariables #-}

module Text.EditDistance.Tests.Properties (tests) where

import Text.EditDistance.EditCosts
import qualified Text.EditDistance.SquareSTUArray as SquareSTUArray
import qualified Text.EditDistance.STUArray as STUArray
import qualified Text.EditDistance.Bits as Bits
import Text.EditDistance.Tests.Framework

import Test.QuickCheck


tests :: [Test]
tests = [ TestGroup "Levenshtein Distance (SquareSTUArray)" sqstu_levenshteinDistanceTests
        , TestGroup "Restricted Damerau-Levenshtein Distance (SquareSTUArray)" sqstu_restrictedDamerauLevenshteinDistanceTests
        , TestGroup "Levenshtein Distance (STUArray)" stu_levenshteinDistanceTests
        , TestGroup "Restricted Damerau-Levenshtein Distance (STUArray)" stu_restrictedDamerauLevenshteinDistanceTests
        , TestGroup "Levenshtein Distance (Bits)" bits_levenshteinDistanceTests
        , TestGroup "Restricted Damerau-Levenshtein Distance (Bits)" bits_restrictedDamerauLevenshteinDistanceTests
        , TestGroup "Levenshtein Distance Crosschecks" levenshteinDistanceCrosscheckTests
        , TestGroup "Restricted Damerau-Levenshtein Distance Crosschecks" restrictedDamerauLevenshteinDistanceCrosscheckTests
        ]
  where
    sqstu_levenshteinDistanceTests                  = standardDistanceTests SquareSTUArray.levenshteinDistance                  interestingCosts (undefined :: BasicEditOperation)
    sqstu_restrictedDamerauLevenshteinDistanceTests = standardDistanceTests SquareSTUArray.restrictedDamerauLevenshteinDistance interestingCosts (undefined :: ExtendedEditOperation)
    stu_levenshteinDistanceTests                    = standardDistanceTests STUArray.levenshteinDistance                        interestingCosts (undefined :: BasicEditOperation)
    stu_restrictedDamerauLevenshteinDistanceTests   = standardDistanceTests STUArray.restrictedDamerauLevenshteinDistance       interestingCosts (undefined :: ExtendedEditOperation)
    bits_levenshteinDistanceTests                   = standardDistanceTests (const Bits.levenshteinDistance)                    defaultEditCosts (undefined :: BasicEditOperation)
    bits_restrictedDamerauLevenshteinDistanceTests  = standardDistanceTests (const Bits.restrictedDamerauLevenshteinDistance)   defaultEditCosts (undefined :: ExtendedEditOperation)
    
    levenshteinDistanceCrosscheckTests 
      = crossCheckTests [ ("SquareSTUArray", SquareSTUArray.levenshteinDistance defaultEditCosts)
                        , ("STUArray",       STUArray.levenshteinDistance defaultEditCosts)
                        , ("Bits",           Bits.levenshteinDistance) ]
                        (undefined :: BasicEditOperation)
    
    restrictedDamerauLevenshteinDistanceCrosscheckTests 
      = crossCheckTests [ ("SquareSTUArray", SquareSTUArray.restrictedDamerauLevenshteinDistance defaultEditCosts)
                        , ("STUArray",       STUArray.restrictedDamerauLevenshteinDistance defaultEditCosts)
                        , ("Bits",           Bits.restrictedDamerauLevenshteinDistance) ]
                        (undefined :: ExtendedEditOperation)


interestingCosts :: EditCosts
interestingCosts = EditCosts {
    deletionCost = 1,
    insertionCost = 2,
    substitutionCost = 3, -- Can't be higher than deletion + insertion
    transpositionCost = 3 -- Can't be higher than deletion + insertion
}


crossCheckTests :: forall op. (EditOperation op, Show op) => [(String, String -> String -> Int)] -> op -> [Test]
crossCheckTests named_distances _op_dummy
  = [ Property (name1 ++ " vs. " ++ name2) (\(MkEditedString old new _ :: EditedString op) -> distance1 old new == distance2 old new)
    | (ix1, (name1, distance1)) <- enumerated_named_distances, (ix2, (name2, distance2)) <- enumerated_named_distances, ix2 > ix1 ]
  where
    enumerated_named_distances = [(1 :: Int)..] `zip` named_distances

standardDistanceTests :: forall op. (EditOperation op, Show op) => (EditCosts -> String -> String -> Int) -> EditCosts -> op -> [Test]
standardDistanceTests distance costs _op_dummy
  = [ Property "Self distance is zero" prop_self_distance_zero
    , Property "Pure deletion has the right cost" prop_pure_deletion_cost_correct
    , Property "Pure insertion has the right cost" prop_pure_insertion_cost_correct
    , Property "Single operations have the right cost" prop_single_op_cost_is_distance
    , Property "Cost bound is respected" prop_combined_op_cost_at_least_distance
    ]
  where
    testableDistance = distance costs

    prop_self_distance_zero str
      = testableDistance str str == 0
    prop_pure_deletion_cost_correct str
      = testableDistance str "" == (deletionCost costs) * length str
    prop_pure_insertion_cost_correct str
      = testableDistance "" str == (insertionCost costs) * length str
    prop_single_op_cost_is_distance (MkEditedString old new ops :: EditedString op)
      = (length old > 2) ==> testableDistance old new == editCost costs ops || old == new
    prop_combined_op_cost_at_least_distance (MkEditedString old new ops :: EditedString [op])
      = not (containsTransposition ops) ==> testableDistance old new <= editCost costs ops