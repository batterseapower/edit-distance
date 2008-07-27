module Main where

import Text.EditDistance.Tests.Framework
import Text.EditDistance.Tests.Properties

import System.Environment
import System.Exit
import System.Random
import Test.QuickCheck


testConfig :: Config
testConfig | True      = defaultConfig { configMaxTest = 1000 }
           | otherwise = defaultConfig

main :: IO ()
main = do
    args <- getArgs
    (rnd, seed) <- case args of 
        []         -> mkStdGenWithKnownSeed
        [old_seed] -> let seed = read old_seed
                      in return (mkStdGen seed, seed)
        _          -> error $ "Unrecognised arguments " ++ unwords args
    
    putStrLn $ "Running tests with seed " ++ show seed
    result <- runTests testConfig rnd tests
    exitWith $ if result
               then ExitSuccess
               else ExitFailure 1

mkStdGenWithKnownSeed :: IO (StdGen, Int)
mkStdGenWithKnownSeed = do
    seed <- randomIO
    return (mkStdGen seed, seed)