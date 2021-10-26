module Main where

import System.Environment
import Test.Tasty (defaultMain, testGroup, TestTree)


-- import all the files where tests of various files/features are located
import ParserTest
import EvalTest
import Ast
import Eval
import Parser

-- Look at the comments in ExampleTest.hs for details on how to write tests


-- this will set up how to run this test suite

main = 
    do 
        setEnv "TASTY_TIMEOUT" "40s"
        setEnv "TASTY_QUICKCHECK_TESTS" "1000" --TODO: I never trust less than 10000
        setEnv "TASTY_QUICKCHECK_MAX_SIZE" "50"
        defaultMain testSuite
        unsetEnv "TASTY_TIMEOUT"
        unsetEnv "TASTY_QUICKCHECK_TESTS"
        unsetEnv "TASTY_QUICKCHECK_MAX_SIZE"


testSuite =
  testGroup
    "allTests"
    [
    EvalTest.tests,
    ParserTest.tests
    ]
