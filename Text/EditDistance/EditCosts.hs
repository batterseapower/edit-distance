{-# OPTIONS_GHC -funbox-strict-fields #-}

module Text.EditDistance.EditCosts (
    Costs(..),
    EditCosts(..), deletionCost, insertionCost, substitutionCost, transpositionCost,
    defaultEditCosts, isDefaultEditCosts
  ) where

data Costs a = ConstantCost !Int
             | VariableCost (a -> Int)

{-# INLINE cost #-}
cost :: Costs a -> a -> Int
cost (ConstantCost i) _ = i
cost (VariableCost f) x = f x

data EditCosts = EditCosts {
    deletionCosts :: Costs Char,
    insertionCosts :: Costs Char,
    substitutionCosts :: Costs (Char, Char),
    transpositionCosts :: Costs (Char, Char)
  }

{-# INLINE deletionCost #-}
deletionCost :: EditCosts -> Char -> Int
deletionCost ec deleted = cost (deletionCosts ec) deleted

{-# INLINE insertionCost #-}
insertionCost :: EditCosts -> Char -> Int
insertionCost ec inserted = cost (insertionCosts ec) inserted

{-# INLINE substitutionCost #-}
substitutionCost :: EditCosts -> Char -> Char -> Int
substitutionCost ec old new = cost (substitutionCosts ec) (old, new)

{-# INLINE transpositionCost #-}
transpositionCost :: EditCosts -> Char -> Char -> Int
transpositionCost ec backwards forwards = cost (transpositionCosts ec) (backwards, forwards)

defaultEditCosts :: EditCosts
defaultEditCosts = EditCosts {
    deletionCosts = ConstantCost 1,
    insertionCosts = ConstantCost 1,
    substitutionCosts = ConstantCost 1,
    transpositionCosts = ConstantCost 1
}

isDefaultEditCosts :: EditCosts -> Bool
isDefaultEditCosts (EditCosts { deletionCosts = ConstantCost 1, insertionCosts = ConstantCost 1, substitutionCosts = ConstantCost 1, transpositionCosts = ConstantCost 1 }) = True
isDefaultEditCosts _ = False