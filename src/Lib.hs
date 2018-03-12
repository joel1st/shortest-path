{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Lib ( 
	handleLog,
	helpMessage, 
	decodePerson, 
	retrieveVal, 
	getCommand, 
	getArg,
	trim,
	initCache,
	convText ) where
import System.Environment
import Data.Aeson
import qualified Data.List as L
import qualified Data.Traversable as T
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Text
import qualified Data.HashMap.Strict as Hm
handleLog :: String -> IO ()
handleLog test = putStrLn test

helpMessage :: String
helpMessage = "Description of how to use the project"

decodePerson hi = decode (C.pack hi) :: Maybe Value

unwrapObj :: Maybe(Value) -> Object
unwrapObj val = case val of
	Just (Object o) -> o
	_ -> Hm.empty 

retrieveVal :: String -> Maybe(Value) -> Object
retrieveVal key val =  unwrapObj $ Hm.lookup (pack key) (unwrapObj val)

convText :: String -> Text
convText input = pack input

getCommand :: Text -> String
getCommand input 
	| isPrefixOf "create" input = "create"
	| isPrefixOf "update" input = "update"
	| isPrefixOf "calculate" input = "calculate"
	| otherwise            = ""

getArg cmd input = input L.\\ cmd

trim :: String -> String
trim val = unpack $ strip $ pack val

createEmptyCache = DijkstraCache Nothing Nothing False

initCache graph = Hm.map (\x -> createEmptyCache) (unwrapObj graph)

data DijkstraCache = DijkstraCache { distance :: Maybe Float, nodePath :: Maybe String, finished :: Bool } deriving Show


-- Dijkstra Algorithm
shortestDistance :: String -> String -> Object -> Value -> Int
shortestDistance startPoint endPoint cache graph = 5
 where
 node = retrieveVal startPoint (Just graph)
 -- closestNode = filter node 



-- 1. init cache with empty vals and set starting point to have a val of 0
-- 2. find lowest val in cache that doesn't have finished set to true - if the lowest val is the endPoint, return the val.
-- 3. use graph to determine distances from starting point (calc = cache point + graph distance) - update cache and set to true after completing.
-- 4.Feed new cache into point 2.
