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
	nextNode
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

unwrapObj val = case val of
	Just (x) -> x
	_ -> error "Invalid unwrap" 

retrieveVal key val =  unwrapObj $ Hm.lookup key (unwrapObj val)

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
shortestDistance :: String -> String -> DijkstraCache -> GraphNodes -> Int
shortestDistance startPoint endPoint cache graph = 5
 where
 node = retrieveVal startPoint (Just graph)
 -- closestNode = filter node 


-- 2. find lowest val in cache that doesn't have finished set to true - if the lowest val is the endPoint, return the val.
nextNode :: DijkstraCache -> String 
nextNode cache = snd minNode
  where 
  validNodes = Hm.filter (\v -> 
  	finished v == False && 
	nodePath v /= Nothing &&
	distance v /= Nothing
	) cache
  minNode = L.minimum $ L.map (\(key, val) -> 
	    case distance val of
	      Just(dist) -> (dist, key)
	      _ -> (0, key) -- this should never happen because of filter above
	    ) $ toList validNodes

-- 3. use graph to determine distances from starting point (calc = cache point + graph distance) - update cache and set to true after completing.
calcVals :: String -> GraphNodes -> DijkstraCache -> DijkstraCache 
calcVals key graph cache = cache 
  where 
  graphNode = retrieveVal key (Just graph) 
  dijkstraNode = retrieveVal key (Just cache)
  connectedNodesDistance = L.map (\(key, val) -> 
      (key, val + (unwrapObj $ distance dijkstraNode))
    ) $ toList graphNode 
  
-- update cache where value is smaller than the cache 



  
	


-- 4.Feed new cache into point 2.
