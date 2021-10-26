module ExampleTest where

import Test.Tasty (testGroup)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)


import Exec
import Eval

-- provide tests that show your run/eval works

tests = testGroup "ExampleTest" 
  [
  testCase  "Euler problem 1" $ do res <- runFile "example/euler1.mylang"
                                   assertEqual "" (Ok $ undefined) $  res
  -- ...
  ]

