import Data.Maybe
import Data.Tuple

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Foldable (minimumBy)
import Data.List (intercalate, foldl')
import Data.Ord (comparing)

main = interact (showPath . shortestPath . parseInput)

-- I/O Logic
parseInput :: String -> (Node, Node, Graph)
parseInput input = (1, end, graph)
  where
    inputLines = lines input
    end = (map read $ words (head inputLines)) !! 0
    edgeData = map parseEdge $ tail inputLines
    initNodes = initializeNodes end
    graph = foldl' addEdge' initNodes edgeData

parseEdge :: String -> (Node, Node, Distance)
parseEdge edgeLine = (src, dst, weight)
  where
    edgeList = map read $ words edgeLine

    src = edgeList !! 0
    dst = edgeList !! 1
    weight = edgeList !! 2

showPath :: Maybe [Node] -> String
showPath Nothing = "-1"
showPath (Just route) = intercalate " " $ map show route

type Node = Int

type Distance = Int

type Edge = (Node, Distance)

type Route = [Node]

-- only allows the minimum weight edge between any two nodes
type Graph = Map Node [Edge]

-- tracking which nodes we've gotten to
type VisitedNodes = Set Node

-- set will be used as a priority queue, along with prev/curr nodes
type PriorityQueue = Set (Distance, (Node, Maybe Node))

-- on the optimal path from start to end, what's the preceding node for a given node?
type PreviousMap = Map Node Node

-- to declutter the function types
type DijkstraStructs = (VisitedNodes, PriorityQueue, PreviousMap)

testGraph :: Graph
testGraph = addEdge (1, 2, 2) . addEdge (2, 5, 5) . addEdge (2, 3, 4) . addEdge (1, 4, 1) . addEdge (4, 3, 3) . addEdge (3, 5, 1) $ initializeNodes 5

bigGraph :: Graph
bigGraph = foldl' addEdge' (initializeNodes 20) bigEdges

bigEdges :: [(Int, Int, Int)]
bigEdges = [(a, b, c) | a <- [1..20], b <- [1..20], c <- [1..10]]

-- for each node, edges
initializeNodes :: Int -> Graph
initializeNodes n = foldl' createNode Map.empty [1 .. n]

createNode :: Graph -> Node -> Graph
createNode g node = Map.insert node [] g

nodesOf :: Graph -> [Node]
nodesOf graph = Map.keys graph

-- create symmetric edge, undirected graph
addEdge :: (Node, Node, Distance) -> Graph -> Graph
addEdge (a, b, w) = add a b w . add b a w
  where
    add a b w graph = let edges = fromJust $ Map.lookup a graph in Map.insert a ((b, w) : edges) graph

-- create symmetric edge, undirected graph
addEdge' :: Graph -> (Node, Node, Distance) -> Graph
addEdge' = flip addEdge

retracePath :: Node -> PreviousMap -> Route
retracePath node prevMap = case Map.lookup node prevMap of 
  Nothing         -> [node]
  (Just nextNode) -> retracePath nextNode prevMap ++ [node] -- adding in reverse order 
  
shortestPath :: (Node, Node, Graph) -> Maybe Route
shortestPath (start, end, graph) = case maybePrevMap of
  Nothing -> Nothing
  Just prevMap -> Just (retracePath end prevMap)
  where
    -- this encodes the shortest path from start -> end, but it has to be decoded 
    maybePrevMap = dijkstra graph end (initVisitedNodes, initPq, initPrev)

    -- for path-tracking, we'll have a previous map so we can later retrace the path from the end node
    initPrev = Map.empty

    -- priority queue init
    initPq = Set.insert (0, (start, Nothing)) Set.empty

    initVisitedNodes = Set.empty

dijkstra :: Graph -> Node -> DijkstraStructs -> Maybe PreviousMap
dijkstra graph target (visitedNodes, pq, prev)
  | emptyPrioQueue = Nothing
  | alreadyVisited = dijkstra graph target (visitedNodes, nextPq, prev)
  | reachedTarget = Just nextPrevMap
  | otherwise = dijkstra graph target (updatedVisitedNodes, neighborPq, nextPrevMap)
  where
    -- we've exhausted the search along the nodes we can reach when this is true
    emptyPrioQueue = Set.null pq

    -- greedy: find the edge leading to the tentatively closest node, and remove it
    ((distance, (nearestNode, maybePrevNode)), nextPq) = Set.deleteFindMin pq
    updatedVisitedNodes = Set.insert nearestNode visitedNodes

    -- if the current node has been visited already, we will skip
    alreadyVisited = nearestNode `Set.member` visitedNodes

    -- for path-tracking
    nextPrevMap = case maybePrevNode of 
      Nothing -> prev
      Just prevNode -> Map.insert nearestNode prevNode prev

    -- if the nearest node is the target node, then we're done. the path is encoded in the PreviousMap
    reachedTarget = nearestNode == target

    -- otherwise, keep searching. add all outgoing edges from current node into priority queue
    neighbors = (Map.!) graph nearestNode
    neighborPq = foldr (\(toNode, w) -> Set.insert (distance + w, (toNode, Just nearestNode))) nextPq neighbors
