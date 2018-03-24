{-# LANGUAGE OverloadedStrings #-}
module Calculate (
  initCache,
  shortestDistance
) where
import qualified Data.List as L
import qualified Data.Traversable as T
import qualified Data.HashMap.Strict as Hm
import qualified Data.Either as E
import Types

-- 1. cache is used to store computation results of dijkstra's algorithim
-- init cache with empty vals and set starting point to have a val of 0
initCache :: String -> GraphNodes -> DijkstraCache
initCache startKey graph = updateCache (DijkstraNode (Just 0) (Just startKey) False) startKey emptyCache
 where
 emptyCache = Hm.map (\v -> DijkstraNode Nothing Nothing False) graph

-- 2. Dijkstra Algorithm
shortestDistance :: Either String String -> String -> DijkstraCache -> GraphNodes -> Either String Float
shortestDistance currentPoint endPoint cache graph
  | E.isLeft currentPoint = Left $ E.fromLeft "" currentPoint
  | E.fromRight "" currentPoint == endPoint = formatResult (E.fromRight "" currentPoint) endPoint cache graph
  | otherwise = shortestDistance nextPoint endPoint currentPointCache graph 
    where 
    currentPointCache = calcDistancesFromNode (E.fromRight "" currentPoint) graph cache
    nextPoint = nextNode currentPointCache 

formatResult :: String -> String -> DijkstraCache -> GraphNodes -> Either String Float
formatResult currentPoint endPoint cache graph = formatted
    where
    res = fromMaybe Nothing $ distance <$> (Hm.lookup currentPoint cache)
    formatted = case res of
      Just(flt) -> Right flt
      Nothing -> Left "Distance not found"

-- 2. find lowest val in cache that doesn't have finished set to true
nextNode :: DijkstraCache -> Either String String
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
      then Left "Requested endpoints are not connected"
    else Right $ snd $ L.minimum $ L.map (\(key, val) ->
      case distance val of
        Just(dist) -> (dist, key)
        _ -> error "Bad filter logic" -- this should never happen because of filter above
      ) $ nodesAsList

-- 3. use graph to determine distances from starting point (calc = cache point + graph distance) 
-- update cache and set processed to true for the point after completing.
calcDistancesFromNode :: String -> GraphNodes -> DijkstraCache -> DijkstraCache 
calcDistancesFromNode key graph cache = newProcessedCache 
  where 
  graphNode = Hm.lookup key graph
  dijkstraNode = Hm.lookup key cache
  distancesFromNode = determineDistance graphNode dijkstraNode
  distancesAppliedToCache = Hm.mapWithKey (applyDistanceToNode distancesFromNode) cache
  newProcessedCache = updateCache (
      DijkstraNode (fromMaybe Nothing $ distance <$> dijkstraNode) (fromMaybe Nothing $ nodePath <$> dijkstraNode) True
    ) key distancesAppliedToCache 

determineDistance :: Maybe GraphNode -> Maybe DijkstraNode -> DistanceFromNode
determineDistance Nothing _ = []
determineDistance _ Nothing = []
determineDistance (Just graphNode) (Just dijkstraNode) = L.map (\(key, val) -> 
      (key, (+ val) <$> (distance dijkstraNode))
    ) $ Hm.toList graphNode
  
applyDistanceToNode :: DistanceFromNode -> String -> DijkstraNode -> DijkstraNode
applyDistanceToNode distanceFromNode key val = newNode
  where
  cacheElem = L.find (\updatedNode -> fst updatedNode == key) distanceFromNode
  newNode = case cacheElem of
    Just(elem) -> 
      if distance val == Nothing || snd elem < (distance val)
        then DijkstraNode (snd elem) (Just key) False
      else val
    _ -> val

fromMaybe def (Just a) = a
fromMaybe def Nothing = Nothing 

updateCache val key graph = Hm.adjust (\v -> val) key graph
