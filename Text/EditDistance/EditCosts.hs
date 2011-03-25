{-# OPTIONS_GHC -funbox-strict-fields #-}

module Text.EditDistance.EditCosts where

data EditCosts = EditCosts {
    deletionCost :: !Int,
    insertionCost :: !Int,
    substitutionCost :: !(Either !Int (Char -> Char -> Int)),
    transpositionCost :: !Int
  }
  deriving (Eq)


defaultEditCosts :: EditCosts
defaultEditCosts = EditCosts {
    deletionCost = 1,
    insertionCost = 1,
    substitutionCost = 1,
    transpositionCost = 1
}