{-# LANGUAGE OverloadedStrings #-}
module Input ( 
  feedbackLoop,
) where
import qualified Data.HashMap.Strict as Hm
import qualified Data.Text as T
import qualified Data.List as L
import Types
import Calculate
import CreateUpdate

feedbackLoop state = do 
  input <- getLine
  let [cmd, arg] = handleInput input
  case cmd of
    "create" -> do 
      handleCreate arg state
    "update" -> do 
      handleUpdate arg state
    "calculate" -> do 
      handleCalculate arg state
    "state" -> do 
      print state
      feedbackLoop state
    _ -> do 
      putStrLn "Invalid Command"
      feedbackLoop state

handleCreate arg state = do
  let decodedInput = decodeInput arg 
  case decodedInput of
    Just(input) -> do 
      let sanitisedInput = sanitiseGraph input
      putStrLn "Graph Created: Type `state` to view"
      feedbackLoop (Just sanitisedInput)
    _ -> do
      putStrLn "Invalid graph format"

handleUpdate arg state = do
  case state of
    Just(val) -> do 
      let decodedInput = decodeInput arg 
      case decodedInput of
        Just(updates) -> do 
          let newState = deepMergeGraph val updates 
          print newState
          let sanitisedInput = sanitiseGraph newState 
          putStrLn "Graph Updated: Type `state` to view"
          feedbackLoop (Just sanitisedInput)
        _ -> do 
          putStrLn "Invalid update parameters"
          feedbackLoop state
    _ -> do
      putStrLn "Please create your graph before attempting to update"
      feedbackLoop state

handleCalculate arg state = do
  let [start, end] = T.splitOn "->" $ T.pack arg
  let startPoint = trim $ T.unpack start
  let endPoint = trim $ T.unpack end 

  case state of
    Just(val) -> do 
      let cache = initCache startPoint val
      let startValid = Hm.member startPoint cache
      let endValid = Hm.member endPoint cache
      putStrLn "Shortest Distance :"
      print $ shortestDistance startPoint endPoint cache val
      feedbackLoop state
    _ -> do
      putStrLn "Please provide data to use"
      feedbackLoop state

handleInput input = [cmd, arg]
  where
    trimInput = trim input
    cmd = getCommand $ T.pack trimInput 
    arg = trim $ getArg cmd trimInput 

getCommand :: T.Text -> String
getCommand input 
  | T.isPrefixOf "create" input = "create"
  | T.isPrefixOf "update" input = "update"
  | T.isPrefixOf "calculate" input = "calculate"
  | T.isPrefixOf "state" input = "state"
  | otherwise            = ""

getArg cmd input = input L.\\ cmd

trim :: String -> String
trim val = T.unpack $ T.strip $ T.pack val

