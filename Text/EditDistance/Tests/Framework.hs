{-# LANGUAGE PatternGuards, ExistentialQuantification #-}

module Text.EditDistance.Tests.Framework (
        Test(..), runTests
    ) where

import Test.QuickCheck
import System.Random
import Control.Monad
import Data.List


-- The following is a custom test framework of sorts
data Test = forall a. Testable a => Property String a
          | TestGroup String [Test]

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


-- The following somewhat ripped out of the QuickCheck source code so that
-- I can customise the random number generator used to do the checking
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