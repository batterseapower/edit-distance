{-# LANGUAGE PatternGuards, PatternSignatures #-}

module Text.EditDistance.Tests.Properties (tests) where

import Text.EditDistance
import Text.EditDistance.Tests.Framework

import Test.QuickCheck


tests = [ TestGroup "Damerau-Levenshtein Distance" restrictedDamerauLevenshteinDistanceTests
        ]


testableCosts = EditCosts {
    deletionCost = 1,
    insertionCost = 2,
    substitutionCost = 3, -- Can't be higher than deletion + insertion
    transpositionCost = 3 -- Can't be higher than deletion + insertion
}


restrictedDamerauLevenshteinDistanceTests = [ Property "Self distance is zero" prop_self_distance_zero
                                            , Property "Pure deletion has the right cost" prop_pure_deletion_cost_correct
                                            , Property "Pure insertion has the right cost" prop_pure_insertion_cost_correct
                                            , Property "Single operations have the right cost" prop_single_op_cost_is_distance
                                            , Property "Cost bound is respected" prop_combined_op_cost_at_least_distance
                                            ]
  where
    testableDistance = restrictedDamerauLevenshteinDistance testableCosts

    prop_self_distance_zero str
      = testableDistance str str == 0
    prop_pure_deletion_cost_correct str
      = testableDistance str "" == (deletionCost testableCosts) * length str
    prop_pure_insertion_cost_correct str
      = testableDistance "" str == (insertionCost testableCosts) * length str
    prop_single_op_cost_is_distance (MkEditedString old new ops :: ExtendedSinglyEditedString)
      = (length old > 2) ==> testableDistance old new == editCost testableCosts ops || old == new
    prop_combined_op_cost_at_least_distance (MkEditedString old new ops :: ExtendedMultiplyEditedString)
      = testableDistance old new <= editCost testableCosts ops