{-# LANGUAGE PatternGuards, PatternSignatures, ScopedTypeVariables #-}

module Text.EditDistance.Tests.Properties (tests) where

import Text.EditDistance.EditCosts
import qualified Text.EditDistance.SquareSTUArray as SquareSTUArray
import qualified Text.EditDistance.Bits as Bits
import Text.EditDistance.Tests.Framework

import Test.QuickCheck


tests :: [Test]
tests = [ TestGroup "Levenshtein Distance (SquareSTUArray)" stu_levenshteinDistanceTests
        , TestGroup "Restricted Damerau-Levenshtein Distance (SquareSTUArray)" stu_restrictedDamerauLevenshteinDistanceTests
        , TestGroup "Levenshtein Distance (Bits)" bits_levenshteinDistanceTests
        , TestGroup "Restricted Damerau-Levenshtein Distance (Bits)" bits_restrictedDamerauLevenshteinDistanceTests
        , TestGroup "Levenshtein Distance Crosschecks" levenshteinDistanceCrosschecks
        , TestGroup "Restricted Damerau-Levenshtein Distance Crosschecks" restrictedDamerauLevenshteinDistanceCrosschecks
        ]
  where
    stu_levenshteinDistanceTests                  = standardDistanceTests  SquareSTUArray.levenshteinDistance                  interestingCosts (undefined :: BasicEditOperation)
    stu_restrictedDamerauLevenshteinDistanceTests = standardDistanceTests  SquareSTUArray.restrictedDamerauLevenshteinDistance interestingCosts (undefined :: ExtendedEditOperation)
    bits_levenshteinDistanceTests                  = standardDistanceTests (const Bits.levenshteinDistance)                    defaultEditCosts (undefined :: BasicEditOperation)
    bits_restrictedDamerauLevenshteinDistanceTests = standardDistanceTests (const Bits.restrictedDamerauLevenshteinDistance)   defaultEditCosts (undefined :: ExtendedEditOperation)
    levenshteinDistanceCrosschecks = [ crossCheckTest "SquareSTUArray" (SquareSTUArray.levenshteinDistance defaultEditCosts)
                                                      "Bits"           Bits.levenshteinDistance
                                                      (undefined :: BasicEditOperation) ]
    restrictedDamerauLevenshteinDistanceCrosschecks = [ crossCheckTest "SquareSTUArray" (SquareSTUArray.restrictedDamerauLevenshteinDistance defaultEditCosts)
                                                                       "Bits"           Bits.restrictedDamerauLevenshteinDistance
                                                                       (undefined :: ExtendedEditOperation) ]


interestingCosts :: EditCosts
interestingCosts = EditCosts {
    deletionCost = 1,
    insertionCost = 2,
    substitutionCost = 3, -- Can't be higher than deletion + insertion
    transpositionCost = 3 -- Can't be higher than deletion + insertion
}


crossCheckTest :: forall op. (EditOperation op, Show op) => String -> (String -> String -> Int) -> String -> (String -> String -> Int) -> op -> Test
crossCheckTest name1 distance1 name2 distance2 _op_dummy
  = Property (name1 ++ " vs. " ++ name2) (\(MkEditedString old new _ :: EditedString op) -> distance1 old new == distance2 old new)

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
      = transpositionRestricted ops ==> testableDistance old new <= editCost costs ops