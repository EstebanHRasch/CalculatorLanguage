module Exec where

import Data.Set (Set)
import qualified Data.Set as Set

import Ast
import Eval
import Parser
import ParserMonad

data LangOut = 
    Error String
  -- ^ retuned when there is a runtime error
  -- first String is the error message
  | Value Val
  -- ^ retuned when the program runs successfully and return a value
  deriving (Show, Eq)

exec :: String -> Either String (State, Val)
exec s = execParse s


runFile :: FilePath -> IO LangOut
runFile path = 
  do program <- readFile path
     return $ case exec program of
        Left str -> Error str
     	Right (s, val) -> Value val

parseFile path = 
  do program <- readFile path
     return $ parse parsemodule program
