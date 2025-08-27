module Main (main) where

import SinkNodeGraph
import qualified Data.Set as Set
import Data.List (intercalate, sort)
import System.Environment (getArgs, getProgName)
import System.IO

parseEdge :: String -> (String, String)
parseEdge line = 
    case words line of
        [from, to] -> (from, to)
        _ -> error $ "Invalid edge format: " ++ line

buildGraphFromFile :: FilePath -> IO Graph
buildGraphFromFile filePath = do
    content <- readFile filePath
    let edges = map parseEdge (lines content)
    return $ foldl addEdge empty edges

queryFromFile :: Graph -> FilePath -> IO [Set.Set String]
queryFromFile graph filePath = do
    content <- readFile filePath
    let queries = lines content
    return $ map (leavesFrom graph) queries

formatSet :: Set.Set String -> String
formatSet s = "{" ++ intercalate "," (sort $ Set.toList s) ++ "}"

main :: IO ()
main = do
    args <- getArgs
    case args of
        [edgesFile, queriesFile] -> do
            graph <- buildGraphFromFile edgesFile
            results <- queryFromFile graph queriesFile
            mapM_ (putStrLn . formatSet) results
        _ -> do
            progName <- getProgName
            hPutStrLn stderr $ "Usage: " ++ progName 
                             ++ " <edges_file> <queries_file>"