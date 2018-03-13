{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.HashMap.Strict as Hm
import Lib
import Data.Text

main :: IO ()
main = do
  handleLog helpMessage 
  feedbackLoop Nothing

feedbackLoop state = do 
  print state
  input <- getLine
  let [cmd, arg] = handleInput input
  case cmd of
    "create" -> do 
      handleCreate arg state
    "update" -> do 
      handleUpdate arg state
    "calculate" -> do 
      handleCalculate arg state
    _ -> do 
      putStrLn "Invalid Command"
      feedbackLoop state

handleCreate arg state = do
  let decodedInput = decodeInput arg 
  case decodedInput of
    Just(input) -> do 
      let sanitisedInput = sanitiseGraph input
      feedbackLoop (Just sanitisedInput)
    _ -> do
      putStrLn "Invalid graph format"

handleUpdate arg state = do
  putStrLn "Update logic"
  case state of
    Just(val) -> do 
      let decodedInput = decodeInput arg 
      case decodedInput of
        Just(updates) -> do 
          let newState = deepMergeGraph val updates 
          print newState
          let sanitisedInput = sanitiseGraph newState 
          feedbackLoop (Just sanitisedInput)
        _ -> do 
          putStrLn "Invalid update parameters"
          feedbackLoop state
    _ -> do
      putStrLn "Please create your graph before attempting to update"
      feedbackLoop state

handleCalculate arg state = do
  putStrLn "Calc logic"
  let [start, end] = splitOn "->" $ pack arg
  let startPoint = trim $ unpack start
  let endPoint = trim $ unpack end 

  case state of
    Just(val) -> do 
      let cache = initCache startPoint val
      let startValid = Hm.member startPoint cache
      putStrLn "Valid Start"
      print startValid 
      let endValid = Hm.member endPoint cache
      putStrLn "Valid End"
      print endValid 
      print cache
      print cache
      print $ nextNode cache
      print $ shortestDistance startPoint endPoint cache val
      -- print $ cache
      feedbackLoop state
    _ -> do
      putStrLn "Please provide data to use"
      feedbackLoop state

handleInput input = [cmd, arg]
  where
    trimInput = trim input
    cmd = getCommand $ pack trimInput 
    arg = trim $ getArg cmd trimInput 


