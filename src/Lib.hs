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
	convText ) where
import System.Environment
import Data.Aeson
import qualified Data.List as L
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Text
import qualified Data.HashMap.Strict as Hm
handleLog :: String -> IO ()
handleLog test = putStrLn test

helpMessage :: String
helpMessage = "hey what up with the world"

decodePerson hi = decode (C.pack hi) :: Maybe Value

retrieveVal :: Maybe(Value) -> String -> Maybe(Value)
retrieveVal val key = case val of
    Just (Object o) -> case Hm.lookup (pack key) o of
                  Just resp -> Just resp
		  _ -> Nothing
    _ -> Nothing

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

-- shortestDistance :: Text -> Text -> Value -> Number
-- shortestDistance start end graph = 5

