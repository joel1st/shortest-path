{-# LANGUAGE OverloadedStrings #-}
module Main where

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
	    	     let decodedInput = decodePerson arg 
		     print $ retrieveVal decodedInput "age"
	    	     feedbackLoop decodedInput
	    "update" -> do 
		     putStrLn "Update logic"
		     feedbackLoop state
	    "calculate" -> do 
	    	     putStrLn "Calc logic"
		     feedbackLoop state
	    _ -> do 
	    	     putStrLn "Invalid Command"
		     feedbackLoop state

handleInput input = [cmd, arg]
	where
	 trimInput = trim input
	 cmd = getCommand $ convText trimInput 
	 arg = trim $ getArg cmd trimInput 


