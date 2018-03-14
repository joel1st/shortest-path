{-# LANGUAGE OverloadedStrings #-}
module Main where
import Input

main :: IO ()
main = do
  putStrLn helpMessage 
  feedbackLoop Nothing

helpMessage = title ++ commands ++ createHelp ++ updateHelp ++ calculateHelp ++ stateHelp
title = "\n~~~Shortest Point Interactive CLI~~~" 
commands = "\n  Commands: [create, update, calculate, state]"
createHelp = "\n    create args: [Json in format: {\"a\": {\"b\": 5}}]"
updateHelp = "\n    update args: [Json in format: {\"a\": {\"b\": 5}}]"
calculateHelp = "\n    calculate args: [a->b]"
stateHelp = "\n    state args: none"


