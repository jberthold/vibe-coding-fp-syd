{-# LANGUAGE OverloadedStrings #-}
module LogParser
    ( LogEvent(..)
    , TestName
    , TestResult(..)
    , WorkerId
    , parseLogLine
    , parseLogFile
    ) where

import Data.List (stripPrefix, isInfixOf)
import Data.Maybe (mapMaybe)

type TestName = String
type WorkerId = String

data TestResult = Passed | Failed | Skipped
    deriving (Show, Eq)

data LogEvent
    = TestStarted TestName
    | TestFinished WorkerId TestResult TestName
    deriving (Show, Eq)

-- | Parse a single log line into a LogEvent
parseLogLine :: String -> Maybe LogEvent
parseLogLine line
    | Just testName <- parseTestStart line = Just (TestStarted testName)
    | Just (workerId, result, testName) <- parseTestEnd line = 
        Just (TestFinished workerId result testName)
    | otherwise = Nothing

-- | Parse test start line (lines that don't start with [gw)
parseTestStart :: String -> Maybe TestName
parseTestStart line
    | take 3 line /= "[gw" 
      && not (null (words line))
      && not ('=' `elem` take 5 line)  -- Skip separator lines
      && not ("platform" `elem` words (take 20 line))  -- Skip platform info
      && "::" `isInfixOf` line  -- Test names typically contain ::
      = Just (strip line)
    | otherwise = Nothing

-- | Parse test end line (lines starting with [gw)
parseTestEnd :: String -> Maybe (WorkerId, TestResult, TestName)
parseTestEnd line = do
    rest1 <- stripPrefix "[gw" line
    let (workerIdPart, rest2) = break (== ']') rest1
    rest3 <- stripPrefix "] [" rest2
    let (_, rest4) = break (== '%') rest3
    rest5 <- stripPrefix "%] " rest4
    let (resultStr, testNamePart) = break (== ' ') rest5
    result <- parseResult resultStr
    let testName = strip (drop 1 testNamePart)
    return (workerIdPart, result, testName)

-- | Parse test result string
parseResult :: String -> Maybe TestResult
parseResult "PASSED" = Just Passed
parseResult "FAILED" = Just Failed
parseResult "SKIPPED" = Just Skipped
parseResult _ = Nothing

-- | Remove leading and trailing whitespace
strip :: String -> String
strip = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

-- | Parse an entire log file
parseLogFile :: String -> [LogEvent]
parseLogFile content = mapMaybe parseLogLine (lines content)