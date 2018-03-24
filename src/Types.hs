module Types ( 
  DijkstraNode(..),
  DijkstraCache,
  GraphNode,
  GraphNodes,
  DistanceFromNode
  ) where
import Data.HashMap.Strict

data DijkstraNode = DijkstraNode { distance :: Maybe Float, nodePath :: Maybe String, finished :: Bool } deriving Show
type DijkstraCache = HashMap String DijkstraNode

type GraphNode = HashMap String Float
type GraphNodes = HashMap String GraphNode

type DistanceFromNode = [(String, Maybe(Float))]
