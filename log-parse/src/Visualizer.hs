module Visualizer
    ( TestState(..)
    , TestExecution(..)
    , buildExecutionTimeline
    , visualizeExecution
    ) where

import LogParser (LogEvent(..), TestName, TestResult(..))
import Data.List (isInfixOf)

data TestState = Running | Finished TestResult
    deriving (Show, Eq)

data TestExecution = TestExecution
    { testName :: TestName
    , startTime :: Int  -- Order of start
    , endTime :: Maybe Int  -- Order of finish (Nothing if still running)
    , result :: Maybe TestResult
    } deriving (Show, Eq)

-- | Build execution timeline from log events
buildExecutionTimeline :: [LogEvent] -> [TestExecution]
buildExecutionTimeline events = 
    let (executions, _) = foldl processEvent ([], 0) events
    in reverse executions
  where
    processEvent :: ([TestExecution], Int) -> LogEvent -> ([TestExecution], Int)
    processEvent (execs, time) event = case event of
        TestStarted name -> 
            let newExec = TestExecution name time Nothing Nothing
            in (newExec : execs, time + 1)
        TestFinished _ testResult name ->
            let updatedExecs = map updateExecution execs
            in (updatedExecs, time + 1)
          where
            updateExecution exec
                | testName exec == name = 
                    exec { endTime = Just time, result = Just testResult }
                | otherwise = exec

-- | Visualize test execution as ASCII art
visualizeExecution :: [TestExecution] -> String
visualizeExecution executions = 
    let maxTime = maximum (map getEndTime executions)
        sortedExecs = executions -- Already in start order
    in unlines $ map (renderTestLine maxTime) sortedExecs
  where
    getEndTime exec = case endTime exec of
        Just t -> t
        Nothing -> startTime exec + 10  -- Assume running tests take 10 units

renderTestLine :: Int -> TestExecution -> String
renderTestLine _ exec =
    let startPos = startTime exec
        name = testName exec
        -- Extract just the test name part (after ::)
        shortName = case reverse (words (filter (/= ' ') name)) of
            [] -> name
            (_:_) -> if "::" `isInfixOf` name 
                           then drop 2 $ dropWhile (/= ':') $ dropWhile (/= ':') name
                           else name
        
        (endChar, actualEndTime) = case (endTime exec, result exec) of
            (Just _, Just Passed) -> ('.', endTime exec)
            (Just _, Just Failed) -> ('X', endTime exec)
            (Just _, Just Skipped) -> ('S', endTime exec)
            (Just _, Nothing) -> ('.', endTime exec)  -- Finished but no result
            (Nothing, _) -> ('-', Nothing)  -- Still running
        
        prefix = replicate startPos ' ' ++ "*"
        
        lineLength = case actualEndTime of
            Just t -> max 0 (t - startPos)
            Nothing -> 10  -- Fixed length for running tests
            
        line = case actualEndTime of
            Just _ -> replicate (max 0 lineLength) '-' ++ [endChar]
            Nothing -> replicate lineLength '-'
            
    in prefix ++ line ++ " " ++ shortName