module Main where

import Text.EditDistance

import System.IO
import System.Exit
import System.Posix.IO
import System.Time      ( ClockTime(..), getClockTime )
import System.Random
import System.Process
import Control.Monad
import Control.Exception
import Control.Concurrent       ( forkIO, threadDelay )
import Control.Parallel.Strategies      ( rnf )

mAX_STRING_SIZE :: Int
mAX_STRING_SIZE = 100

time :: IO a -> IO Float
time action = do 
    TOD s1 ps1 <- getClockTime
    action
    TOD s2 ps2 <- getClockTime
    return $ (fromIntegral (s2 - s1) + (fromIntegral (ps2 - ps1) / 10^12))

augment :: Monad m => (a -> m b) -> [a] -> m [(a, b)]
augment fx xs = liftM (zip xs) $ mapM fx xs

sample :: (EditCosts -> String -> String -> Int) -> (Int, Int) -> IO Float
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
    time $ loop 1000 $ evaluate (distance defaultEditCosts string1 string2)

loop :: Monad m => Int -> m a -> m ()
loop n act = sequence_ (replicate n act)

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
    samples <- augment (sample restrictedDamerauLevenshteinDistance)
                       [(i, j) | i <- [0..mAX_STRING_SIZE]
                               , j <- [0..mAX_STRING_SIZE]]
    
    writeFile "data.plot" (toGnuPlotFormat samples)
    
    {-
    (gp_stdin_rd_fd, gp_stdin_wr_fd) <- createPipe
    gp_stdin_rd <- fdToHandle gp_stdin_rd_fd
    gp_stdin_wr <- fdToHandle gp_stdin_wr_fd
    
    {-
    export AQUATERM_REPORT_TIMING=1
export AQUATERM_PATH=
export GNUTERMAPP=
export GNUTERM=aqua
    -}
    {-
    let env = [ ("AQUATERM_REPORT_TIMING", "1")
              , ("AQUATERM_PATH", "/Applications/AquaTerm.app")
              , ("GNUTERMAPP", "/Applications/AquaTerm.app")
              , ("GNUTERM", "aqua")
              ]-}
    gp_pid <- runProcess "tee" [] Nothing Nothing  (Just gp_stdin_rd) Nothing Nothing --(Just gp_stdin_rd) (Just stdout) (Just stderr)
    
    --forkIO ()
    hPutStr gp_stdin_wr gnuPlotScript
    --hPutStr gp_stdin_wr "ls\n"
    --threadDelay 20000000
    gp_exit_code <- waitForProcess gp_pid
    case gp_exit_code of
            ExitSuccess -> putStrLn "Plotted at 'data.ps'"
            ExitFailure err_no -> putStrLn $ "Failed! Error code " ++ show err_no
    
    --hClose gp_stdin_rd
    hClose gp_stdin_wr
    
    --closeFd gp_stdin_rd_fd
    closeFd gp_stdin_wr_fd
    -}