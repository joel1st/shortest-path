{-# LANGUAGE OverloadedStrings #-}
module Main where
import Input

main :: IO ()
main = do
  putStrLn helpMessage 
  feedbackLoop Nothing

helpMessage = "Description of how to use the project"
