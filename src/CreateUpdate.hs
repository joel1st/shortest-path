{-# LANGUAGE OverloadedStrings #-}
module CreateUpdate ( 
  decodeInput,
  deepMergeGraph,
	sanitiseGraph
	) where
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as Hm
import Types

decodeInput json = Aeson.decode (C.pack json) :: Maybe GraphNodes

-- Sanitise input so that every connection has a corresponding connection.
-- Eg. {"a": {"c": 5}} is converted to {"a": {"c": 5}, "c": {"a": 5}} for internal use.
sanitiseGraph :: GraphNodes -> GraphNodes
sanitiseGraph graph = Hm.foldlWithKey' (\a k v ->
    deepMergeGraph	(deepMergeGraph a (Hm.singleton k v)) (buildInverseGraph k v)
  ) (Hm.empty :: GraphNodes) graph
 
buildInverseGraph :: String -> GraphNode -> GraphNodes
buildInverseGraph key node = Hm.foldlWithKey' (\a k v -> 
    Hm.insert k (Hm.singleton key v) a
  ) (Hm.empty :: GraphNodes) node

-- deep merge graph
deepMergeGraph :: GraphNodes -> GraphNodes -> GraphNodes
deepMergeGraph graph graphChanges = Hm.unionWithKey (\k v1 v2 -> Hm.union v2 v1) graph graphChanges

