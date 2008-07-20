module Main where

import Text.EditDistance.Tests.Framework
import Text.EditDistance.Tests.Properties

import Test.QuickCheck


myTest :: (Testable a) => a -> IO ()
myTest | True      = check (defaultConfig { configMaxTest = 300 })
       | otherwise = test -- Quicker

putStrIndented :: Int -> String -> IO ()
putStrIndented indent what = putStr $ (replicate indent ' ') ++ what


runTest :: Int -> Test -> IO ()
runTest indent (Property name the_property) = do
    putStrIndented indent name
    myTest the_property
runTest indent (TestGroup name tests) = do
    putStrIndented indent (name ++ ":")
    mapM_ (runTest (indent + 2)) tests

main :: IO ()
main = mapM_ (runTest 0) tests