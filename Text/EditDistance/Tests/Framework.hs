{-# LANGUAGE PatternGuards, ExistentialQuantification #-}

module Text.EditDistance.Tests.Framework where

import Text.EditDistance

import Test.QuickCheck
import System.Random
import Control.Monad
import Data.Char


data Test = forall a. Testable a => Property String a
          | TestGroup String [Test]


instance Arbitrary Char where
    arbitrary     = choose ('\32', '\128')
    coarbitrary c = variant (ord c `rem` 4)


class Arbitrary ops => EditOperation ops where
    edit :: Arbitrary a => [a] -> ops -> Gen [a]
    editCost :: EditCosts -> ops -> Int
    isTransposition :: ops -> Bool
    transpositionRestricted :: ops -> Bool

instance EditOperation op => EditOperation [op] where
   edit = foldM edit
   editCost costs = sum . map (editCost costs)
   isTransposition = any isTransposition
   transpositionRestricted ops = all (\(isLast, op) -> isLast || not (isTransposition op)) ((True:repeat False) `zip` (reverse ops))


data EditedString ops = MkEditedString {
    oldString :: String,
    newString :: String,
    operations :: ops
}

instance Show ops => Show (EditedString ops) where
    show (MkEditedString old_string new_string ops) = show old_string ++ " ==> " ++ show new_string ++ " (by " ++ show ops ++ ")"

instance EditOperation ops => Arbitrary (EditedString ops) where
    arbitrary = do
        old_string <- arbitrary
        edit_operations <- arbitrary
        new_string <- edit old_string edit_operations
        return $ MkEditedString {
            oldString = old_string,
            newString = new_string,
            operations = edit_operations
        }


data ExtendedEditOperation = Deletion
                           | Insertion
                           | Substitution
                           | Transposition
                           deriving (Enum, Bounded, Show)

instance Arbitrary ExtendedEditOperation where
    arbitrary = fmap toEnum $ choose (fromEnum (minBound :: ExtendedEditOperation), fromEnum (maxBound :: ExtendedEditOperation))
    coarbitrary op = variant (fromEnum op)

instance EditOperation ExtendedEditOperation where
    edit str op = do
        gen <- rand
        let max_split_ix | Transposition <- op = length str - 1
                         | otherwise           = length str
            (split_ix, _) = randomR (1, max_split_ix) gen
            (str_l, str_r) = splitAt split_ix str
            non_null = not $ null str
            transposable = length str > 1
        case op of
            Deletion | non_null -> return $ init str_l ++ str_r
            Insertion | non_null -> do
                new_ch <- arbitrary
                return $ str_l ++ new_ch : str_r
            Insertion | otherwise -> fmap singleton arbitrary   -- Need special case because randomR (1, 0) is undefined
            Substitution | non_null -> do
                new_ch <- arbitrary
                return $ init str_l ++ new_ch : str_r
            Transposition | transposable -> do                  -- Need transposable rather than non_null because randomR (1, 0) is undefined
                return $ init str_l ++ head str_r : last str_l : tail str_r
            _ -> return str
      where
        singleton :: a -> [a]
        singleton x = [x]
    
    editCost costs Deletion      = deletionCost costs
    editCost costs Insertion     = insertionCost costs
    editCost costs Substitution  = substitutionCost costs
    editCost costs Transposition = transpositionCost costs

    isTransposition Transposition = True
    isTransposition _             = False

    transpositionRestricted _ = True


-- This all really sucks but I can't think of something better right now
newtype BasicEditOperation = MkBasic ExtendedEditOperation

instance Show BasicEditOperation where
    show (MkBasic x) = show x

instance Arbitrary BasicEditOperation where
    arbitrary = fmap (MkBasic . toEnum) $ choose (fromEnum (minBound :: ExtendedEditOperation), fromEnum (maxBound :: ExtendedEditOperation) - 1)
    coarbitrary (MkBasic op) = variant (fromEnum op)

instance EditOperation BasicEditOperation where
    edit str (MkBasic op) = edit str op
    editCost costs (MkBasic op) = editCost costs op
    isTransposition _ = False
    transpositionRestricted _ = True