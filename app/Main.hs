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
	-- print $ retrieveVal "age" decodedInput
	feedbackLoop decodedInput 

handleUpdate arg state = do
	putStrLn "Update logic"
	feedbackLoop state

handleCalculate arg state = do
	putStrLn "Calc logic"
	case state of
	    Just(val) -> do 
		let cache = initCache "a" val
		let startValid = Hm.member "a" cache
		putStrLn "Valid Start"
		print startValid 
		let endValid = Hm.member "b" cache
		putStrLn "Valid End"
		print endValid 
		print cache
		print cache
		print $ nextNode cache
		print $ shortestDistance "a" "b" cache val
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


