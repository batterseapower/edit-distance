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
import Control.Monad
import Control.Exception
--import Control.Concurrent       ( forkIO, threadDelay )
import Control.Parallel.Strategies      ( NFData, rnf )

sTRING_SIZE_STEP, mAX_STRING_SIZE :: Int
sTRING_SIZE_STEP = 3
mAX_STRING_SIZE = 21

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
\unset key\n\
\set dgrid3d\n\
\set hidden3d\n\
\set pm3d map\n\
\splot \"data.plot\" using 1:2:3\n\
\quit\n"

toGnuPlotFormat :: (Show a, Show b, Show c) => [((a, b), c)] -> String
toGnuPlotFormat samples = unlines (header : map sampleToGnuPlotFormat samples)
  where
    header = "#\tX\tY\tZ"
    sampleToGnuPlotFormat ((a, b), c) = concat ["\t", show a, "\t", show b, "\t", show c]

main :: IO ()
main = do
    let sample_range = [(i, j) | i <- [0,sTRING_SIZE_STEP..mAX_STRING_SIZE]
                               , j <- [0,sTRING_SIZE_STEP..mAX_STRING_SIZE]]
    sqstu_samples <- augment (sample $ SquareSTUArray.restrictedDamerauLevenshteinDistance defaultEditCosts) sample_range
    stu_samples <- augment (sample $ STUArray.restrictedDamerauLevenshteinDistance defaultEditCosts) sample_range
    let paired_samples = sqstu_samples `joinOnKey` stu_samples
        diff_samples = [((i, j), left_time - right_time) | ((i, j), (left_time, right_time)) <- paired_samples]
    
    writeFile "data.plot" (toGnuPlotFormat diff_samples)
    writeFile "plot.script" gnuPlotScript
    
    (_inp, _outp, _err, gp_pid) <- runInteractiveCommand "(cat plot.script | gnuplot); RETCODE=$?; rm plot.script; exit $RETCODE"
    gp_exit_code <- waitForProcess gp_pid
    case gp_exit_code of
            ExitSuccess -> putStrLn "Plotted at 'data.ps'"
            ExitFailure err_no -> putStrLn $ "Failed! Error code " ++ show err_no