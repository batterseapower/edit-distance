module Main where

import Text.EditDistance.Tests.Framework
import Text.EditDistance.Tests.Properties

import Test.QuickCheck


myTest :: (Testable a) => a -> IO ()
myTest | True      = check (defaultConfig { configMaxTest = 1000 })
       | otherwise = test -- Quicker

putStrIndented :: Int -> String -> IO ()
putStrIndented how_much = putStr . indent how_much

putStrLnIndented :: Int -> String -> IO ()
putStrLnIndented how_much = putStrLn . indent how_much

indent :: Int -> String -> String
indent how_much what = (replicate how_much ' ') ++ what


runTest :: Int -> Test -> IO ()
runTest indent_level (Property name the_property) = do
    putStrIndented indent_level (name ++ ": ")
    myTest the_property
runTest indent_level (TestGroup name the_tests) = do
    putStrLnIndented indent_level (name ++ ":")
    mapM_ (runTest (indent_level + 2)) the_tests

main :: IO ()
main = mapM_ (runTest 0) tests