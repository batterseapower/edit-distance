{-# LANGUAGE PatternGuards, ExistentialQuantification #-}

module Text.EditDistance.Tests.Framework where

import Text.EditDistance

import Test.QuickCheck
import System.Random
import Control.Monad
import Data.Char
import Data.List


data Test = forall a. Testable a => Property String a
          | TestGroup String [Test]


instance Arbitrary Char where
    arbitrary     = choose ('\32', '\128')
    coarbitrary c = variant (ord c `rem` 4)


class Arbitrary ops => EditOperation ops where
    edit :: Arbitrary a => [a] -> ops -> Gen [a]
    editCost :: EditCosts -> ops -> Int
    containsTransposition :: ops -> Bool

instance EditOperation op => EditOperation [op] where
   edit = foldM edit
   editCost costs = sum . map (editCost costs)
   containsTransposition = any containsTransposition


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

    containsTransposition Transposition = True
    containsTransposition _             = False


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
    containsTransposition _ = False


-- The following somewhat ripped out of the QuickCheck source
myCheck :: (Testable a) => Config -> StdGen -> a -> IO Bool
myCheck config rnd a = myTests config (evaluate a) rnd 0 0 []

myTests :: Config -> Gen Result -> StdGen -> Int -> Int -> [[String]] -> IO Bool
myTests config gen rnd0 ntest nfail stamps
  | ntest == configMaxTest config = do done "OK, passed" ntest stamps
  | nfail == configMaxFail config = do done "Arguments exhausted after" ntest stamps
  | otherwise               =
      do putStr (configEvery config ntest (arguments result))
         case ok result of
           Nothing    ->
             myTests config gen rnd1 ntest (nfail+1) stamps
           Just True  ->
             myTests config gen rnd1 (ntest+1) nfail (stamp result:stamps)
           Just False -> do
             putStr ( "Falsifiable, after "
                   ++ show ntest
                   ++ " tests:\n"
                   ++ unlines (arguments result)
                    )
             return False
  where
    result      = generate (configSize config ntest) rnd2 gen
    (rnd1,rnd2) = split rnd0

done :: String -> Int -> [[String]] -> IO Bool
done mesg ntest stamps = do
    putStr ( mesg ++ " " ++ show ntest ++ " tests" ++ table )
    return True
 where
  table = display
        . map entry
        . reverse
        . sort
        . map pairLength
        . group
        . sort
        . filter (not . null)
        $ stamps

  display []  = ".\n"
  display [x] = " (" ++ x ++ ").\n"
  display xs  = ".\n" ++ unlines (map (++ ".") xs)

  pairLength xss@(xs:_) = (length xss, xs)
  entry (n, xs)         = percentage n ntest
                       ++ " "
                       ++ concat (intersperse ", " xs)

  percentage n m        = show ((100 * n) `div` m) ++ "%"


-- The follow is a custom test framework of sorts
putStrIndented :: Int -> String -> IO ()
putStrIndented how_much = putStr . indent how_much

putStrLnIndented :: Int -> String -> IO ()
putStrLnIndented how_much = putStrLn . indent how_much

indent :: Int -> String -> String
indent how_much what = (replicate how_much ' ') ++ what


runTest :: Int -> Config -> StdGen -> Test -> IO Bool
runTest indent_level test_config rnd (Property name the_property) = do
    putStrIndented indent_level (name ++ ": ")
    myCheck test_config rnd the_property
runTest indent_level test_config rnd (TestGroup name the_tests) = do
    putStrLnIndented indent_level (name ++ ":")
    fmap and $ mapM (runTest (indent_level + 2) test_config rnd) the_tests

runTests :: Config -> StdGen -> [Test] -> IO Bool
runTests test_config rnd = fmap and . mapM (runTest 0 test_config rnd)