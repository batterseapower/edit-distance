module Main where

import Text.EditDistance.EditCosts
import qualified Text.EditDistance.Bits as Bits
import qualified Text.EditDistance.STUArray as STUArray
import qualified Text.EditDistance.SquareSTUArray as SquareSTUArray

import System.IO
import System.Exit
--import System.Posix.IO
import System.Time      ( ClockTime(..), getClockTime )
import System.Random
import System.Process
import Data.List
import Control.Monad
import Control.Exception
--import Control.Concurrent       ( forkIO, threadDelay )
import Control.Parallel.Strategies      ( NFData, rnf )

sTRING_SIZE_STEP, mAX_STRING_SIZE :: Int
sTRING_SIZE_STEP = 3
mAX_STRING_SIZE = 108

time :: IO a -> IO Float
time action = do 
    TOD s1 ps1 <- getClockTime
    action
    TOD s2 ps2 <- getClockTime
    return $ (fromIntegral (s2 - s1) + (fromIntegral (ps2 - ps1) / 10^(12 :: Int)))

augment :: Monad m => (a -> m b) -> [a] -> m [(a, b)]
augment fx xs = liftM (zip xs) $ mapM fx xs

sample :: NFData a => (String -> String -> a) -> (Int, Int) -> IO Float
sample distance bounds@(i, j) = do
    -- Generate two random strings of length i and j
    gen <- newStdGen
    let (string1, string2_long) = splitAt i (randoms gen)
        string2 = take j string2_long
    
    -- Force the two strings to be evaluated so they don't meddle
    -- with the benchmarking
    evaluate (rnf string1)
    evaluate (rnf string2)
    
    -- Our sample is the time taken to find the edit distance
    putStrLn $ "Sampling " ++ show bounds
    time $ loop 1000 $ evaluate (distance string1 string2)

loop :: Monad m => Int -> m a -> m ()
loop n act = sequence_ (replicate n act)

joinOnKey :: Eq a => [(a, b)] -> [(a, c)] -> [(a, (b, c))]
joinOnKey xs ys = [(x_a, (x_b, y_c)) | (x_a, x_b) <- xs, (y_a, y_c) <- ys, x_a == y_a]

gnuPlotScript :: String
gnuPlotScript = "set term postscript eps enhanced color\n\
\set output \"data.ps\"\n\
\#unset key\n\
\set dgrid3d\n\
\set hidden3d\n\
\#set pm3d map\n\
\#splot \"data.plot\" using 1:2:3\n\
\splot \"data.plot\" using 1:2:3 title \"Bits\" with lines, \"data.plot\" using 1:2:4 title \"STUArray\" with lines, \"data.plot\" using 1:2:5 title \"SquareSTUArray\" with lines\n\
\quit\n"

toGnuPlotFormat :: (Show a, Show b, Show c) => [((a, b), [c])] -> String
toGnuPlotFormat samples = unlines (header : map sampleToGnuPlotFormat samples)
  where
    first_cs = snd $ head samples
    header = "#\tX\tY" ++ concat (replicate (length first_cs) "\tZ")
    sampleToGnuPlotFormat ((a, b), cs) = concat $ intersperse "\t" $ [show a, show b] ++ map show cs

main :: IO ()
main = do
    let sample_range = [(i, j) | i <- [0,sTRING_SIZE_STEP..mAX_STRING_SIZE]
                               , j <- [0,sTRING_SIZE_STEP..mAX_STRING_SIZE]]
    bits_samples  <- augment (sample $ Bits.levenshteinDistance) sample_range
    sqstu_samples <- augment (sample $ SquareSTUArray.levenshteinDistance defaultEditCosts) sample_range
    stu_samples   <- augment (sample $ STUArray.levenshteinDistance defaultEditCosts) sample_range
    let paired_samples = bits_samples `joinOnKey` (stu_samples `joinOnKey` sqstu_samples)
        listified_samples = [((i, j), [a, b, c]) | ((i, j), (a, (b, c))) <- paired_samples]
    
    writeFile "data.plot" (toGnuPlotFormat listified_samples)
    writeFile "plot.script" gnuPlotScript
    
    (_inp, _outp, _err, gp_pid) <- runInteractiveCommand "(cat plot.script | gnuplot); RETCODE=$?; rm plot.script; exit $RETCODE"
    gp_exit_code <- waitForProcess gp_pid
    case gp_exit_code of
            ExitSuccess -> putStrLn "Plotted at 'data.ps'"
            ExitFailure err_no -> putStrLn $ "Failed! Error code " ++ show err_no