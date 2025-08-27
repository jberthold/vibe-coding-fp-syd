module Main (main) where

import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import LogParser (parseLogFile)
import Visualizer (buildExecutionTimeline, visualizeExecution)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> do
            content <- readFile filename
            let events = parseLogFile content
            let timeline = buildExecutionTimeline events
            let visualization = visualizeExecution timeline
            putStr visualization
        [] -> do
            hPutStrLn stderr "Usage: log-parse-exe <log-file>"
            exitFailure
        _ -> do
            hPutStrLn stderr "Usage: log-parse-exe <log-file>"
            exitFailure