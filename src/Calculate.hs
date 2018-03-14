{-# LANGUAGE OverloadedStrings #-}
module Calculate ( 
	retrieveVal, 
	initCache,
	shortestDistance
	) where
import qualified Data.List as L
import qualified Data.Traversable as T
import qualified Data.HashMap.Strict as Hm
import Types

-- 1. cache is used to store computation results of dijkstra's algorithim
-- init cache with empty vals and set starting point to have a val of 0
initCache :: String -> GraphNodes -> DijkstraCache
initCache startKey graph = updateCache (DijkstraNode (Just 0) (Just startKey) False) startKey emptyCache
 where	
 emptyCache = Hm.map (\v -> DijkstraNode Nothing Nothing False) graph

-- 2. Dijkstra Algorithm
shortestDistance :: String -> String -> DijkstraCache -> GraphNodes -> Float
shortestDistance currentPoint endPoint cache graph
  | currentPoint == endPoint = unwrap $ distance $ retrieveVal currentPoint (Just cache)
  | otherwise = shortestDistance nextPoint endPoint currentPointCache graph 
    where 
    currentPointCache = calcDistancesFromNode currentPoint graph cache
    nextPoint = nextNode currentPointCache 

-- 2. find lowest val in cache that doesn't have finished set to true
nextNode :: DijkstraCache -> String 
nextNode cache = minKey
  where 
  validNodes = Hm.filter (\v -> 
      finished v == False && 
      nodePath v /= Nothing &&
      distance v /= Nothing
    ) cache
  nodesAsList = Hm.toList validNodes
  minKey = 
    if L.length nodesAsList == 0 
      then error "The start and end node are not connected"
    else snd $ L.minimum $ L.map (\(key, val) -> 
      case distance val of
        Just(dist) -> (dist, key)
        _ -> error "Bad filter logic" -- this should never happen because of filter above
      ) $ nodesAsList

-- 3. use graph to determine distances from starting point (calc = cache point + graph distance) 
-- update cache and set processed to true for the point after completing.
calcDistancesFromNode :: String -> GraphNodes -> DijkstraCache -> DijkstraCache 
calcDistancesFromNode key graph cache = newProcessedCache 
  where 
  graphNode = retrieveVal key (Just graph) 
  dijkstraNode = retrieveVal key (Just cache)
  distancesFromNode = determineDistance graphNode dijkstraNode
  distancesAppliedToCache = Hm.mapWithKey (applyDistanceToNode distancesFromNode) cache
  newProcessedCache = updateCache (
      DijkstraNode (distance dijkstraNode) (nodePath dijkstraNode) True
    ) key distancesAppliedToCache 

determineDistance :: GraphNode -> DijkstraNode -> DistanceFromNode
determineDistance graphNode dijkstraNode = L.map (\(key, val) -> 
      (key, val + (unwrap $ distance dijkstraNode))
    ) $ Hm.toList graphNode
  
applyDistanceToNode :: DistanceFromNode -> String -> DijkstraNode -> DijkstraNode
applyDistanceToNode distanceFromNode key val = newNode
  where
  cacheElem = L.find (\updatedNode -> fst updatedNode == key) distanceFromNode
  newNode = case cacheElem of
    Just(elem) -> 
      if distance val == Nothing || snd elem < (unwrap $ distance val)
        then DijkstraNode (Just $ snd elem) (Just key) False
      else val
    _ -> val

-- Util
unwrap val = case val of
  Just (x) -> x
  _ -> error "Invalid unwrap" 

retrieveVal key val =  unwrap $ Hm.lookup key (unwrap val)

updateCache val key graph = Hm.adjust (\v -> val) key graph