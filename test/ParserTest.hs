module ParserTest where

import Test.Tasty (testGroup)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)
import Test.Tasty.QuickCheck 

import Ast
import Parser
import ParserMonad
import Eval


-- This will generate random instances of types

instance Arbitrary Expr where
    arbitrary = sized arbitrarySizedExprAst
	
instance Arbitrary Stmt where
    arbitrary = sized arbitrarySizedStmtAst


{-

-- recursively and randomly generate instances up to a given size limit

arbitrarySizedModAst ::  Int -> Gen ModuleAst
arbitrarySizedModAst m | m < 1 = do i    <- elements [-10 .. 10] -- will choose a random Integer
                                    node <- elements [ModuleAst "euler" ([Assignment "x" (ValInt 2), Assignment "y" (ValInt 3), Return (Plus (Var "x") (Var "y"))]) ]  -- so put all the non-recursive Ast expressions here
                                    return $ node
								 
								 
								 
-}							
arbitrarySizedExprAst ::  Int -> Gen Expr
arbitrarySizedExprAst m | m < 1 = do i    <- elements [-10 .. 10] -- will choose a random Integer
                                     x    <- elements [True, False]
                                     y    <- elements ["x", "y", "z"] -- will choose random element from the list
                                     node <- elements [ValInt i,
									                   ValBool x,
													   Var y
													   --Not (ValBool x)
													   ]  -- so put all the non-recursive Ast expressions here
                                     return $ node

arbitrarySizedExprAst m | otherwise = do l <- arbitrarySizedExprAst (m `div` 2)  -- get ast half as big
                                         r <- arbitrarySizedExprAst (m `div` 2)  -- ditto
                                         x <- elements ["x", "y", "z"]   -- will choose random element from the list
                                         node <- elements [Plus l r,     -- list here all your binary Ast constructors
                                                           Minus l r,
                                                           Mult l r,
													                                 Div l r,
								                          					       Mod l r,
                          													       Eq l r,
													                                 Neq l r,
				                          									       GreaterThan l r,
													                                 LessThan l r,
									                          				       GreaterThanEq l r,
													                                 LessThanEq l r,
                                                           And l r,
													                                 Or l r
													       ]
                                         return node
										 
										 


arbitrarySizedStmtAst ::  Int -> Gen Stmt
arbitrarySizedStmtAst m   = do i <- arbitrarySizedExprAst m
                               stmt <- arbitrarySizedRAAst m
                               node <- elements [If i stmt,
                                                 While i stmt]
                               return $ node

                                          {-}
arbitrarySizedStmtAst m | m < 1 = do i <- arbitrarySizedExprAst m -- will choose a random Expr
                                     x <- elements ["x", "y", "z"]
                                     node <- elements [Return i,
                                                       Assignment x i]  -- so put all the non-recursive Ast expressions here
                                     return $ node

arbitrarySizedStmtAst m | otherwise  = do i <- arbitrarySizedExprAst m
                                          x <- elements ["x", "y", "z"]
                                          y <- arbitrarySizedStmtAst (m `div` 2)
                                          z <- arbitrarySizedStmtAst (m `div` 2)
                                          node <- elements [If i y,
								                            While i y,
										                  --  Seq [y],
										                  --  Seq [y,z],
										                --    Seq [y,z,y],
								                            Separator y z]  -- so put all the non-recursive Ast expressions here
                                          return $ node

-}
arbitrarySizedRAAst ::  Int -> Gen Stmt

arbitrarySizedRAAst m = do i <- arbitrarySizedExprAst m -- will choose a random Expr
                           x <- elements ["x", "y", "z"]
                           node <- elements [Return i,
                                             Assignment x i
                                             --Separator (Return i) (Return i)
                                             --If i (Return i),
                                             --While i (Return i)
                                             ]  -- so put all the non-recursive Ast expressions here
                           return $ node



{-
arbitrarySizedStmtAst m | m <= 1     = do i <- arbitrarySizedExprAst m
                                          x <- elements ["x", "y", "z"]
                                          node <- elements [Return i, Assignment x i]
                                          return $ node

-}
-- break in thirds for mix-fix operators which have three separate sub-asts
{-}
arbitrarySizedModuleAst :: Int -> Gen ModuleAst
arbitrarySizedModuleAst m = do i <- elements [-10 .. 10] -- will choose a random Integer, num of total statements
                               x <- elements ["x", "y", "z"]
                               stmts <- [arbitrarySizedStmtAst (i `div` 2) | x <- [0..i-1]]
                               return $ ModuleAst x stmts
-}
--genStmts :: Int -> [Gen Stmt]
-- provide tests that show your parser works

tests = testGroup "ParserTest" 
  [
	    testProperty "parse should return the same AST when fully parenthesized" $
                  ((\ x -> Just (x , "") == (parse parserExpr $ showFullyParenExpr x)) :: Expr -> Bool),
        
        testProperty "parse should return the same AST when pretty printed" $
                  ((\ x -> Just (x , "") == (parse parserExpr $ showPrettyExpr x 0)) :: Expr -> Bool),
  
		testProperty "parse should return the same AST when fully parenthesized" $
                  ((\ x -> Just (x , "") == (parse parseStmt $ showFullyParenStmt x)) :: Stmt -> Bool),
        
        testProperty "parse should return the same AST when pretty printed" $
                  ((\ x -> Just (x , "") == (parse parseStmt $ showPrettyStmt x 0)) :: Stmt -> Bool)
		
		
		{-}
        testProperty "parse should return the same AST when fully parenthesized" $
                  ((\ x -> Just (x , "") == (parse parserSeqStmts $ showFullyParenStmt x)) :: Stmt -> Bool),
        
        testProperty "parse should return the same AST when pretty printed" $
                  ((\ x -> Just (x , "") == (parse parserSeqStmts $ showPrettyStmt x 0)) :: Stmt -> Bool)
		-}
  ]
  
  
