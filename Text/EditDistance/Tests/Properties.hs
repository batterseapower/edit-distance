{-# LANGUAGE PatternGuards, PatternSignatures, ScopedTypeVariables #-}

module Text.EditDistance.Tests.Properties (
        tests
    ) where

import Text.EditDistance.EditCosts
import qualified Text.EditDistance.SquareSTUArray as SquareSTUArray
import qualified Text.EditDistance.STUArray as STUArray
import qualified Text.EditDistance.Bits as Bits
import Text.EditDistance.Tests.EditOperationOntology

import Test.Framework
import Test.Framework.Providers.QuickCheck
import Test.QuickCheck


tests :: [Test]
tests = [ testGroup "Levenshtein Distance (SquareSTUArray)" sqstu_levenshteinDistanceTests
        , testGroup "Restricted Damerau-Levenshtein Distance (SquareSTUArray)" sqstu_restrictedDamerauLevenshteinDistanceTests
        , testGroup "Levenshtein Distance (STUArray)" stu_levenshteinDistanceTests
        , testGroup "Restricted Damerau-Levenshtein Distance (STUArray)" stu_restrictedDamerauLevenshteinDistanceTests
        , testGroup "Levenshtein Distance (Bits)" bits_levenshteinDistanceTests
        , testGroup "Restricted Damerau-Levenshtein Distance (Bits)" bits_restrictedDamerauLevenshteinDistanceTests
        , testGroup "Levenshtein Distance Crosschecks" levenshteinDistanceCrosscheckTests
        , testGroup "Restricted Damerau-Levenshtein Distance Crosschecks" restrictedDamerauLevenshteinDistanceCrosscheckTests
        --, testGroup "Levenshtein Distance Cutoff (Bits)" bits_levenshteinDistanceCutoffTests
        ]
  where
    sqstu_levenshteinDistanceTests                  = standardDistanceTests SquareSTUArray.levenshteinDistance                  interestingCosts (undefined :: BasicEditOperation)
    sqstu_restrictedDamerauLevenshteinDistanceTests = standardDistanceTests SquareSTUArray.restrictedDamerauLevenshteinDistance interestingCosts (undefined :: ExtendedEditOperation)
    stu_levenshteinDistanceTests                    = standardDistanceTests STUArray.levenshteinDistance                        interestingCosts (undefined :: BasicEditOperation)
    stu_restrictedDamerauLevenshteinDistanceTests   = standardDistanceTests STUArray.restrictedDamerauLevenshteinDistance       interestingCosts (undefined :: ExtendedEditOperation)
    bits_levenshteinDistanceTests                   = standardDistanceTests (const Bits.levenshteinDistance)                    defaultEditCosts (undefined :: BasicEditOperation)
    bits_restrictedDamerauLevenshteinDistanceTests  = standardDistanceTests (const Bits.restrictedDamerauLevenshteinDistance)   defaultEditCosts (undefined :: ExtendedEditOperation)
    
    --bits_levenshteinDistanceCutoffTests = [ testProperty "Cutoff vs. Non-Cutoff" (forAll arbitrary (\cutoff -> distanceEqIfBelowProperty cutoff (Bits.levenshteinDistanceCutoff cutoff) Bits.levenshteinDistance defaultEditCosts (undefined :: BasicEditOperation))) ]
    
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
  = [ testProperty (name1 ++ " vs. " ++ name2) (distanceEqProperty distance1 distance2 _op_dummy)
    | (ix1, (name1, distance1)) <- enumerated_named_distances, (ix2, (name2, distance2)) <- enumerated_named_distances, ix2 > ix1 ]
  where
    enumerated_named_distances = [(1 :: Int)..] `zip` named_distances

distanceEqProperty :: (String -> String -> Int) -> (String -> String -> Int) -> op -> EditedString op -> Bool
distanceEqProperty distance1 distance2 _op_dummy (MkEditedString old new _) = distance1 old new == distance2 old new

--distanceEqIfBelowProperty :: (EditOperation op) => Int -> (String -> String -> Int) -> (String -> String -> Int) -> EditCosts -> op -> EditedString op -> Property
--distanceEqIfBelowProperty cutoff distance1 distance2 costs _op_dummy (MkEditedString old new ops) = (editCost costs ops <= cutoff) ==> distance1 old new == distance2 old new

standardDistanceTests :: forall op. (EditOperation op, Show op) => (EditCosts -> String -> String -> Int) -> EditCosts -> op -> [Test]
standardDistanceTests distance costs _op_dummy
  = [ testProperty "Self distance is zero" prop_self_distance_zero
    , testProperty "Pure deletion has the right cost" prop_pure_deletion_cost_correct
    , testProperty "Pure insertion has the right cost" prop_pure_insertion_cost_correct
    , testProperty "Single operations have the right cost" prop_single_op_cost_is_distance
    , testProperty "Cost bound is respected" prop_combined_op_cost_at_least_distance
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