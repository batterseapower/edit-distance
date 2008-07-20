{-# LANGUAGE PatternGuards, PatternSignatures, ScopedTypeVariables #-}

module Text.EditDistance.Tests.Properties (tests) where

import Text.EditDistance
import Text.EditDistance.Tests.Framework

import Test.QuickCheck


tests :: [Test]
tests = [ TestGroup "Levenshtein Distance" levenshteinDistanceTests
        , TestGroup "Restricted Damerau-Levenshtein Distance" restrictedDamerauLevenshteinDistanceTests
        ]
  where
    levenshteinDistanceTests = standardDistanceTests levenshteinDistance (undefined :: BasicEditOperation)
    restrictedDamerauLevenshteinDistanceTests = standardDistanceTests restrictedDamerauLevenshteinDistance (undefined :: ExtendedEditOperation)


testableCosts :: EditCosts
testableCosts = EditCosts {
    deletionCost = 1,
    insertionCost = 2,
    substitutionCost = 3, -- Can't be higher than deletion + insertion
    transpositionCost = 3 -- Can't be higher than deletion + insertion
}


standardDistanceTests :: forall op. (EditOperation op, Show op) => (EditCosts -> String -> String -> Int) -> op -> [Test]
standardDistanceTests distance _op_dummy
  = [ Property "Self distance is zero" prop_self_distance_zero
    , Property "Pure deletion has the right cost" prop_pure_deletion_cost_correct
    , Property "Pure insertion has the right cost" prop_pure_insertion_cost_correct
    , Property "Single operations have the right cost" prop_single_op_cost_is_distance
    , Property "Cost bound is respected" prop_combined_op_cost_at_least_distance
    ]
  where
    testableDistance = distance testableCosts

    prop_self_distance_zero str
      = testableDistance str str == 0
    prop_pure_deletion_cost_correct str
      = testableDistance str "" == (deletionCost testableCosts) * length str
    prop_pure_insertion_cost_correct str
      = testableDistance "" str == (insertionCost testableCosts) * length str
    prop_single_op_cost_is_distance (MkEditedString old new ops :: EditedString op)
      = (length old > 2) ==> testableDistance old new == editCost testableCosts ops || old == new
    prop_combined_op_cost_at_least_distance (MkEditedString old new ops :: EditedString [op])
      = testableDistance old new <= editCost testableCosts ops