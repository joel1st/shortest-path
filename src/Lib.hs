{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Lib ( 
	handleLog,
	helpMessage, 
	decodeInput, 
	retrieveVal, 
	getCommand, 
	getArg,
	trim,
	initCache,
	nextNode,
	shortestDistance,
	deepMergeGraph,
	sanitiseGraph
	) where
import System.Environment
import Data.Aeson
import qualified Data.List as L
import qualified Data.Traversable as T
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Text
import qualified Data.HashMap.Strict as Hm
import Data.HashMap.Strict
handleLog :: String -> IO ()
handleLog test = putStrLn test

helpMessage :: String
helpMessage = "Description of how to use the project"

decodeInput json = decode (C.pack json) :: Maybe GraphNodes

unwrap val = case val of
	Just (x) -> x
	_ -> error "Invalid unwrap" 

retrieveVal key val =  unwrap $ Hm.lookup key (unwrap val)

getCommand :: Text -> String
getCommand input 
	| isPrefixOf "create" input = "create"
	| isPrefixOf "update" input = "update"
	| isPrefixOf "calculate" input = "calculate"
	| otherwise            = ""

getArg cmd input = input L.\\ cmd

trim :: String -> String
trim val = unpack $ strip $ pack val

updateCache val key graph = Hm.adjust (\v -> val) key graph


-- 1. init cache with empty vals and set starting point to have a val of 0
initCache :: String -> GraphNodes -> DijkstraCache
initCache startKey graph = updateCache (DijkstraNode (Just 0) (Just startKey) False) startKey emptyCache
 where	
 emptyCache = Hm.map (\v -> DijkstraNode Nothing Nothing False) graph


data DijkstraNode = DijkstraNode { distance :: Maybe Float, nodePath :: Maybe String, finished :: Bool } deriving Show
type DijkstraCache = HashMap String DijkstraNode

type GraphNode = HashMap String Float
type GraphNodes = HashMap String GraphNode

-- Dijkstra Algorithm
shortestDistance :: String -> String -> DijkstraCache -> GraphNodes -> Float
shortestDistance currentPoint endPoint cache graph
  | currentPoint == endPoint = unwrap $ distance $ retrieveVal currentPoint (Just cache)
  | otherwise = shortestDistance nextPoint endPoint currentPointCache graph 
    where 
    currentPointCache = calcDistancesFromNode currentPoint graph cache
    nextPoint = nextNode currentPointCache 

-- 2. find lowest val in cache that doesn't have finished set to true - if the lowest val is the endPoint, return the val.
nextNode :: DijkstraCache -> String 
nextNode cache = minKey
  where 
  validNodes = Hm.filter (\v -> 
  	finished v == False && 
	nodePath v /= Nothing &&
	distance v /= Nothing
	) cache
  nodesAsList = toList validNodes
  minKey = 
    if L.length nodesAsList == 0 
      then error "The start and end node are not connected"
	else snd $ L.minimum $ L.map (\(key, val) -> 
	    case distance val of
	      Just(dist) -> (dist, key)
	      _ -> error "Bad filter logic" -- this should never happen because of filter above
	    ) $ nodesAsList

-- 3. use graph to determine distances from starting point (calc = cache point + graph distance) - update cache and set to true after completing.
type DistanceFromNode = [(String, Float)]
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
    ) $ toList graphNode
  
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

-- sanitise graph
sanitiseGraph :: GraphNodes -> GraphNodes
sanitiseGraph graph = foldlWithKey' (\a k v ->
	 deepMergeGraph	(deepMergeGraph a (Hm.singleton k v)) (buildInverseGraph k v)
	) (Hm.empty :: GraphNodes) graph
 
buildInverseGraph :: String -> GraphNode -> GraphNodes
buildInverseGraph key node = foldlWithKey' (\a k v -> 
	Hm.insert k (Hm.singleton key v) a
   ) (Hm.empty :: GraphNodes) node

-- deep merge graph
deepMergeGraph :: GraphNodes -> GraphNodes -> GraphNodes
deepMergeGraph graph graphChanges = unionWithKey (\k v1 v2 -> union v2 v1) graph graphChanges

