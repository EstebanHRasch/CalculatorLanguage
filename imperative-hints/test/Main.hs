module Main where

import Test.Tasty

import EvalTest
import ParserTest
import ExampleTest

main = defaultMain testSuite


testSuite =
  testGroup
    "allTests"
    [
    ExampleTest.tests,
    ParserTest.tests,
    ExampleTest.tests
    -- ...
    ]
