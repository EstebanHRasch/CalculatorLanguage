module EvalTest where

import Test.Tasty (testGroup)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)
--import Test.Tasty.QuickCheck 

import Ast
import Ast
import Eval
import Parser
import StateError
import Data.Map (Map)
import qualified Data.Map as Map


s :: State
s = Map.empty
one = Right (s, I 1)
two = Right (s, I 2)
three = Right (s, I 3)
four = Right (s, I 4)
five = Right (s, I 5)
six = Right (s, I 6)
divErr = Left "cannot divide by 0"
modErr = Left "cannot mod by 0"





true = Right (s, B True)
false = Right (s, B False)
boolErr = Left "not the bool we expected"
intErr = Left "not the int we expected"

-- provide tests that show your run/eval works

tests = testGroup "EvalTest" 
  [(
   testCase "Basic Arithmetic" $ 
            do 
              assertEqual "2 + 4 =? "    six    (execParseEs ("2 + 4"))
              assertEqual "2 + 1 =? "    three  (execParseEs ("2 + 1"))

              assertEqual "2 - 1 =? "    one    (execParseEs ("2 - 1"))
              assertEqual "6 - 3 =? "    three  (execParseEs ("6 - 3"))

              assertEqual "6 / 2 =? "    three  (execParseEs ("6 / 2"))
              assertEqual "6 / 0 =? "    divErr (execParseEs ("6 / 0"))

              assertEqual "2 * 3 =? "    six    (execParseEs ("2 * 3"))
              assertEqual "4 * 1 =? "    four   (execParseEs ("4 * 1"))

              assertEqual "4 % 3 =? "    one    (execParseEs ("4 % 3"))
              assertEqual "4 % 0 =? "    modErr (execParseEs ("4 % 0"))
    )  ,(
    testCase "Comparison Operators" $
            do 
                assertEqual "1 < 2 =?"          true       (execParseEs ("1 < 2"))
                assertEqual "2 < 1 =?"          false      (execParseEs ("2 < 1"))
                assertEqual "false < true"      intErr     (execParseEs ("false < true"))
                assertEqual "false < 2 =?"      intErr     (execParseEs ("false < 2"))
                
                assertEqual "1 > 2 =?"          false      (execParseEs ("1 > 2"))
                assertEqual "2 > 1 =?"          true       (execParseEs ("2 > 1"))
                assertEqual "true > true =?"    intErr     (execParseEs ("true > true"))
                assertEqual "false > 2 =?"      intErr     (execParseEs ("false > 2"))

                assertEqual "1 <= 2 =?"         true       (execParseEs ("1 <= 2"))
                assertEqual "2 <= 1 =?"         false      (execParseEs ("2 <= 1"))
                assertEqual "1 <= 1 =?"         true       (execParseEs ("1 <= 1"))
                assertEqual "true <= true"      intErr     (execParseEs ("true <= true"))
                assertEqual "false <= 2 =?"     intErr     (execParseEs ("false <= 2"))

                assertEqual "1 >= 2 =?"         false      (execParseEs ("1 >= 2"))
                assertEqual "2 >= 1 =?"         true       (execParseEs ("2 >= 1"))
                assertEqual "1 >= 1 =?"         true       (execParseEs ("1 >= 1"))
                assertEqual "false >= true"     intErr     (execParseEs ("false >= true"))
                assertEqual "false >= 2 =?"     intErr     (execParseEs ("false >= 2"))

                assertEqual "1 == 1 =?"         true       (execParseEs ("1 == 1"))
                assertEqual "2 == 1 =?"         false      (execParseEs ("2 == 1"))
                assertEqual "1 == (-1) =?"      false      (execParseEs ("1 == (-1)"))
                assertEqual "false == true =?"  false     (execParseEs ("false == true"))
                assertEqual "false == 2 =?"     false     (execParseEs ("false == 2"))

                assertEqual "1 != 2 =?"         true       (execParseEs ("1 != 2"))
                assertEqual "1 != 1 =?"         false      (execParseEs ("1 != 1"))
                assertEqual "false != true =?"  true     (execParseEs ("false != true"))
                assertEqual "1 != true =?"      true     (execParseEs ("1 != true"))
                assertEqual "false != 2 =?"     true     (execParseEs ("false != 2"))

    ) ,(
    testCase "Boolean Operators" $
            do
                assertEqual "true && true =?"   true       (execParseEs ("true && true"))
                assertEqual "true && false =?"  false      (execParseEs ("true && false"))
                assertEqual "false && true =?"  false      (execParseEs ("false && true"))
                assertEqual "false && false =?" false      (execParseEs ("false && false"))
                assertEqual "true && 1 =?"      boolErr    (execParseEs ("true && 1"))
                assertEqual "1 && true =?"      boolErr    (execParseEs ("1 && true"))

                assertEqual "true || true =?"   true       (execParseEs ("true || true"))
                assertEqual "true || false =?"  true       (execParseEs ("true || false"))
                assertEqual "false || true =?"  true       (execParseEs ("false || true"))
                assertEqual "false || false =?" false      (execParseEs ("false || false"))
                assertEqual "true || 1 =?"      boolErr    (execParseEs ("true || 1"))
                assertEqual "1 || true =?"      boolErr    (execParseEs ("1 || true"))

                assertEqual "! false =?"        true       (execParseEs ("! false"))
                assertEqual "! true =?"         false      (execParseEs ("! true"))
                assertEqual "! 5 =?"            boolErr    (execParseEs ("! 5"))
   ) , (
    testCase "Statements" $
            do 
            	-- if
            	assertEqual "if ( true ) { return 5; }; return 3; =?"       five       (execParseSts ("if ( true ) { return 5; }; return 3;"))
            	assertEqual "if ( false ) { return 5; }; return 3; =?"      three      (execParseSts ("if ( false ) { return 5; }; return 3;"))
                assertEqual "if ( 3 == 3 ) { return 5; }; return 3; =?"     five       (execParseSts ("if ( 3 == 3 ) { return 5; }; return 3;"))
            	assertEqual "if ( 3 != 3 ) { return 5; }; return 3; =?"     three      (execParseSts ("if ( 3 != 3 ) { return 5; }; return 3;")) 
  
            	-- while
            	assertEqual "while ( false ) { return 5; }; return 3; =?"   three      (execParseSts ("while ( false ) { return 5; }; return 3;"))
            	assertEqual "while ( 3 != 3 ) { return 5; }; return 3; =?"  three      (execParseSts ("while ( 3 != 3 ) { return 5; }; return 3;"))
                assertEqual "x := (-10); while ( x < 0 ) { x := x + 1; }; return x; =?"  zero (execParseSts ("x := (-10); while ( x < 0 ) { x := x + 1; }; return x;"))

            	-- assignment
            	assertEqual "x := 3; return x; =?"                          three'      (execParseSts ("x := 3; return x;"))
            	assertEqual "x := 1 + 2; return x; =?"                      three'      (execParseSts ("x := 1 + 2; return x;"))
            	assertEqual "x := true && false; return x; =?"              false'      (execParseSts ("x := true && false; return x;"))
            	assertEqual "x := 2; return x + 1;"                         three''      (execParseSts ("x := 2; return x + 1;"))

            	-- seq 
            	assertEqual "if ( true ) { x := 2; x := x + 1; return x; }; return 5; =?"     three'       (execParseSts ("if ( true ) { x := 2; x := x + 1; return x; }; return 5;"))
                assertEqual "x := 3; while ( x != 5 ) { x := x + 1; }; return x; =?"          five'        (execParseSts ("x := 3; while (x != 5) { x := x + 1; }; return x;"))
                assertEqual "x := 0; y := 10; while (x < y) {x:= x + 1; y:= y - 1;}; if (x > 0) {z := x;}; return z + y; =?" ten (execParseSts ("x := 0; y := 10; while (x < y) {x:= x + 1; y:= y - 1;}; if (x > 0) {z := x;}; return z + y;"))

            	-- assignment
            	assertEqual "x := 5; return x; =?"                          five'      (execParseSts ("x := 5; return x;"))
            	assertEqual "x := 5; x := 3; return x; =?"                  three'      (execParseSts ("x := 5; x := 3; return x;"))
            	assertEqual "x := 5; x := x - 2; return x; =?"              three'      (execParseSts ("x := 5; x := x - 2; return x;"))
            	assertEqual "x := 2; y := 3; return x + y; =?"              five''       (execParseSts ("x := 2; y := 3; return x + y;"))
            	assertEqual "x := true; y := false; return x && y;"         false''      (execParseSts ("x := true; y := false; return x && y;"))
            	assertEqual "x := true; return ! x;"                        false'''      (execParseSts ("x := true; return ! x;"))
      )
   

  ]

s' :: State
s' = Map.insert "x" (I 3) (Map.empty)
s'' :: State
s'' = Map.insert "x" (B False) (Map.empty)
s''' :: State
s''' = Map.insert "x" (I 2) (Map.empty)
s'''' :: State
s'''' = Map.insert "x" (I 5) (Map.empty)
s''''' :: State
s''''' = Map.insert "y" (I 3) s'''
boolS :: State
boolS = Map.insert "x" (B True) (Map.empty)
boolS' :: State
boolS' = Map.insert "y" (B False) boolS
three' =  Right (s', I 3)
false' = Right (s'', B False)
three'' = Right (s''', I 3)
five' = Right (s'''', I 5)
five'' = Right (s''''', I 5)
false'' = Right (boolS', B False)
false''' = Right (boolS, B False)

zero = Right (Map.insert "x" (I 0) Map.empty, I 0)

crazy = Map.insert "x" (I 5) Map.empty
crazy' = Map.insert "y" (I 5) crazy
crazy'' = Map.insert "z" (I 5) crazy'
ten = Right (crazy'', I 10)