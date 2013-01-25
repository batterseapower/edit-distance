module Main where

import Text.EditDistance.EditCosts
import Text.EditDistance.MonadUtilities
import qualified Text.EditDistance as BestEffort
import qualified Text.EditDistance.Bits as Bits
import qualified Text.EditDistance.STUArray as STUArray
import qualified Text.EditDistance.SquareSTUArray as SquareSTUArray

import System.IO
import System.Exit
--import System.Posix.IO
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Random
import System.Process
import System.Mem
import Data.List
import Control.Monad
import Control.Exception
--import Control.Concurrent       ( forkIO, threadDelay )
import Control.DeepSeq      ( NFData, rnf )

sTRING_SIZE_STEP, mAX_STRING_SIZE :: Int
sTRING_SIZE_STEP = 3
mAX_STRING_SIZE = 108

getTime :: IO Double
getTime = realToFrac `fmap` getPOSIXTime

time :: IO a -> IO Double
time action = do 
    ts1 <- getTime
    action
    ts2 <- getTime
    return $ ts2 - ts1

augment :: Monad m => (a -> m b) -> [a] -> m [(a, [b])]
augment fx xs = liftM (zip xs) $ mapM (liftM (\b -> [b]) . fx) xs

sample :: NFData a => (String -> String -> a) -> (Int, Int) -> IO Double
sample distance bounds@(i, j) = do
    -- Generate two random strings of length i and j
    gen <- newStdGen
    let (string1, string2_long) = splitAt i (randoms gen)
        string2 = take j string2_long
    
    -- Force the two strings to be evaluated so they don't meddle
    -- with the benchmarking
    evaluate (rnf string1)
    evaluate (rnf string2)
    
    -- Don't want junk from previous runs causing a GC during the test
    performGC

    -- Our sample is the time taken to find the edit distance
    putStrLn $ "Sampling " ++ show bounds
    time $ loop (100000 `div` (1 + i + j)) $ evaluate (distance string1 string2) >> return ()

loop :: Monad m => Int -> m () -> m ()
loop n act = loopM_ 1 n (const act)

joinOnKey :: Eq a => [(a, [b])] -> [(a, [b])] -> [(a, [b])]
joinOnKey xs ys = [(x_a, (x_b ++ y_c)) | (x_a, x_b) <- xs, (y_a, y_c) <- ys, x_a == y_a]

gnuPlotScript :: [String] -> String
gnuPlotScript titles = "set term postscript eps enhanced color\n\
\set output \"data.ps\"\n\
\#unset key\n\
\set dgrid3d\n\
\set hidden3d\n\
\#set pm3d map\n\
\#splot \"data.plot\" using 1:2:3\n\
\splot " ++ splot_script ++ "\n\
\quit\n"
  where
    --splot_script = "\"data.plot\" using 1:2:3 title \"Bits\" with lines, \"data.plot\" using 1:2:4 title \"STUArray\" with lines, \"data.plot\" using 1:2:5 title \"SquareSTUArray\" with lines"
    splot_script = intercalate ", " ["\"data.plot\" using 1:2:" ++ show i ++ " title " ++ show title ++ " with lines" | (i, title) <- [3..] `zip` titles]

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
        sample_titles = ["Bits", "STUArray", "SquareSTUArray", "Best effort"]
        sample_fns = [Bits.levenshteinDistance, SquareSTUArray.levenshteinDistance defaultEditCosts, STUArray.levenshteinDistance defaultEditCosts, BestEffort.levenshteinDistance defaultEditCosts]
        --sample_fns = [Bits.restrictedDamerauLevenshteinDistance, SquareSTUArray.restrictedDamerauLevenshteinDistance defaultEditCosts, STUArray.restrictedDamerauLevenshteinDistance defaultEditCosts, BestEffort.restrictedDamerauLevenshteinDistance defaultEditCosts]
    sampless <- forM sample_fns $ \sample_fn -> augment (sample sample_fn) sample_range
    let listified_samples = foldr1 joinOnKey sampless
    
    writeFile "data.plot" (toGnuPlotFormat listified_samples)
    writeFile "plot.script" (gnuPlotScript sample_titles)
    
    (_inp, _outp, _err, gp_pid) <- runInteractiveCommand "(cat plot.script | gnuplot); RETCODE=$?; rm plot.script; exit $RETCODE"
    gp_exit_code <- waitForProcess gp_pid
    case gp_exit_code of
            ExitSuccess -> putStrLn "Plotted at 'data.ps'"
            ExitFailure err_no -> putStrLn $ "Failed! Error code " ++ show err_no