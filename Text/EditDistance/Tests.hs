module Main where

import Text.EditDistance.Tests.Properties

import Test.Framework

import Data.Monoid


main :: IO ()
main = defaultMain $ map (plusTestOptions $ mempty { topt_maximum_generated_tests = Just 1000 }) tests