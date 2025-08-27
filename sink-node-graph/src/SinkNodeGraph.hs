{-# LANGUAGE OverloadedStrings #-}

module SinkNodeGraph
    ( Graph
    , empty
    , addEdge
    , leavesFrom
    ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import Data.Set (Set)
import Control.Monad.State

type NodeLabel = String
type AdjacencyList = Map NodeLabel (Set NodeLabel)

newtype Graph = Graph AdjacencyList
    deriving (Show, Eq)

empty :: Graph
empty = Graph Map.empty

addEdge :: Graph -> (NodeLabel, NodeLabel) -> Graph
addEdge (Graph adjList) (from, to) = 
    Graph $ Map.insertWith Set.union from (Set.singleton to) 
          $ Map.insertWith Set.union to Set.empty adjList

leavesFrom :: Graph -> NodeLabel -> Set NodeLabel
leavesFrom (Graph adjList) startNode = 
    computeLeavesWithMemo adjList startNode

computeLeavesWithMemo :: AdjacencyList -> NodeLabel -> Set NodeLabel
computeLeavesWithMemo adjList startNode =
    case Map.lookup startNode adjList of
        Nothing -> Set.empty
        Just _ -> evalState (dfsLeaves startNode) (Set.empty, Map.empty)
  where
    dfsLeaves :: NodeLabel -> State (Set NodeLabel, Map NodeLabel (Set NodeLabel)) (Set NodeLabel)
    dfsLeaves node = do
        (visited, memo) <- get
        case Map.lookup node memo of
            Just cachedResult -> return cachedResult
            Nothing -> 
                if Set.member node visited
                    then return Set.empty
                    else do
                        modify (\(v, m) -> (Set.insert node v, m))
                        case Map.lookup node adjList of
                            Nothing -> return Set.empty
                            Just neighbors -> 
                                if Set.null neighbors
                                    then do
                                        let result = Set.singleton node
                                        modify (\(v, m) -> (v, Map.insert node result m))
                                        return result
                                    else do
                                        childResults <- mapM dfsLeaves (Set.toList neighbors)
                                        let result = Set.unions childResults
                                        modify (\(v, m) -> (v, Map.insert node result m))
                                        return result