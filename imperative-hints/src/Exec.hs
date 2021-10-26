module Exec where

import Data.Set (Set)
import qualified Data.Set as Set

import Ast
import Eval
import Parser
import ParserMonad



data LangOut = 
    ParseError -- ^ retuned when the string could not be parsed
  | RuntimeError String
  -- ^ retuned when there is a runtime error
  -- first String is the error message
  | Ok Val
  -- ^ retuned when the program runs successfully and return a value
  deriving (Show, Eq)

  
-- | execute the program as a string and get the result
exec :: String -> LangOut
exec s = undefined

runFile :: FilePath -> IO LangOut
runFile path = 
  do program <- readFile path
     return $ exec program

parseFile path = 
  do program <- readFile path
     return $ parse parsemodule program
