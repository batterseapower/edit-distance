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
    deletionCosts :: Costs Char,             -- ^ Cost of deleting the specified character from the left string
    insertionCosts :: Costs Char,            -- ^ Cost of inserting the specified characters into the right string
    substitutionCosts :: Costs (Char, Char), -- ^ Cost of substituting a character from the left string with one from the right string -- with arguments in that order.
    transpositionCosts :: Costs (Char, Char) -- ^ Cost of moving one character backwards and the other forwards -- with arguments in that order.
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