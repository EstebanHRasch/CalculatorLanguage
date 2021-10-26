module Ast where

import Data.Map (Map)
import qualified Data.Map as Map
import HelpShow

-- procedure, parameter names and a body
data ModuleAst = ModuleAst String [Stmt]  deriving (Eq,Show)

data Stmt =
  Assignment String Expr
  | Separator Stmt Stmt
  | Seq [Stmt]
  | If Expr Stmt
  | While Expr Stmt
  | Return Expr deriving Eq
   
data Expr = 
{-should include: 
Vars
procedure calls 
LitteralInt, all the int operations
LitteralBool, all the bool operations
comparisons: <, <=, ==, !=
and perhaps: Nil, Cons and list operations
-}
  Var String | ValInt Integer | ValBool Bool
  | Plus Expr Expr | Minus Expr Expr | Mult Expr Expr
  | Div Expr Expr | Mod Expr Expr
  | Or Expr Expr | And Expr Expr | Not Expr 
  | LessThan Expr Expr | GreaterThan Expr Expr 
  | LessThanEq Expr Expr | GreaterThanEq Expr Expr
  | Eq Expr Expr | Neq Expr Expr deriving Eq


instance Show Stmt where
  -- display the ast in a readable way
  show ast = showPrettyStmt ast 0

{-- instance Eq Stmt where  -- you can just derive this if you want
  Stmt == Stmt = Stmt == Stmt deriving Eq --}
  
instance Show Expr where
  -- display the ast in a readable way
  show ast = showPrettyExpr ast 0

{-- instance Eq Expr where  -- you can just derive this if you want
  Expr == Expr = Expr == Expr deriving Eq --}
  

  
-- | output the fully parenthesized statement

showFullyParen :: ModuleAst
                  -> String  -- ^ the fully parenthesized string representing the input Ast
showFullyParen (ModuleAst str stmts) = "(" ++ str ++ (showFullyParenStmt (Seq stmts)) ++ ")"

showFullyParenStmt :: Stmt
                -> String  -- ^ the fully parenthesized string representing the input Ast
showFullyParenStmt (Assignment str expr) = str ++ " := " ++ (showFullyParenExpr expr) ++ ";"
showFullyParenStmt (If expr stmts) = "if " ++ (showFullyParenExpr expr) ++ "{" ++ (showFullyParenStmt stmts) ++ "};"
showFullyParenStmt (While expr stmts) = "while (" ++ (showFullyParenExpr expr) ++ ") {" ++ (showFullyParenStmt stmts)  ++ "};"
showFullyParenStmt (Return expr) = "return " ++ (showFullyParenExpr expr) ++ ";"
showFullyParenStmt (Separator l r) = (showFullyParenStmt l) ++ ";" ++ (showFullyParenStmt r) 
showFullyParenStmt (Seq lst) = case lst of 
                                   [x] -> (showFullyParenStmt x)
                                   (x:xs) -> (showFullyParenStmt x) ++ (showFullyParenStmt (Seq xs))
                                   _ -> ""
                                  




showFullyParenExpr :: Expr
                -> String  -- ^ the fully parenthesized string representing the input Ast
showFullyParenExpr (Var str) = "(" ++ str ++ ")"
showFullyParenExpr (ValInt i) = "(" ++ (show i) ++ ")"
showFullyParenExpr (ValBool True) = "( true )"
showFullyParenExpr (ValBool False) = "( false )"
showFullyParenExpr (Plus l r) = "(" ++ (showFullyParenExpr l) ++ "+" ++ (showFullyParenExpr r) ++ ")"
showFullyParenExpr (Minus l r) = "(" ++ (showFullyParenExpr l) ++ "-" ++ (showFullyParenExpr r) ++ ")"
showFullyParenExpr (Mult l r) = "(" ++ (showFullyParenExpr l) ++ "*" ++ (showFullyParenExpr r) ++ ")"
showFullyParenExpr (Div l r) = "(" ++ (showFullyParenExpr l) ++ "/" ++ (showFullyParenExpr r) ++ ")"
showFullyParenExpr (Mod l r) = "(" ++ (showFullyParenExpr l) ++ "%" ++ (showFullyParenExpr r) ++ ")"
showFullyParenExpr (Or l r) = "(" ++ (showFullyParenExpr l) ++ "||" ++ (showFullyParenExpr r) ++ ")"
showFullyParenExpr (And l r) = "(" ++ (showFullyParenExpr l) ++ "&&" ++ (showFullyParenExpr r) ++ ")"
showFullyParenExpr (Not expr) = "(" ++ " ! " ++ (showFullyParenExpr expr) ++ ")"
showFullyParenExpr (LessThan l r) = "(" ++ (showFullyParenExpr l) ++ "<" ++ (showFullyParenExpr r) ++ ")"
showFullyParenExpr (GreaterThan l r) = "(" ++ (showFullyParenExpr l) ++ ">" ++ (showFullyParenExpr r) ++ ")"
showFullyParenExpr (LessThanEq l r) = "(" ++ (showFullyParenExpr l) ++ "<=" ++ (showFullyParenExpr r) ++ ")"
showFullyParenExpr (GreaterThanEq l r) = "(" ++ (showFullyParenExpr l) ++ ">=" ++ (showFullyParenExpr r) ++ ")"
showFullyParenExpr (Eq l r) = "(" ++ (showFullyParenExpr l) ++ "==" ++ (showFullyParenExpr r) ++ ")"
showFullyParenExpr (Neq l r) = "(" ++ (showFullyParenExpr l) ++ "!=" ++ (showFullyParenExpr r) ++ ")"


-- | provide a nice show with minimal parentheses
showPretty :: ModuleAst  -- ^ The Ast to show
            -> Integer  -- ^ The precedence of the root expression, see the doc for 'HelpShow.parenthesize' for more detail
            -> String  -- ^ the minimally parenthesized string representing the input Ast
showPretty (ModuleAst str stmts) i =  parenthesize 0 i $ str ++ "{" ++ (showPrettyStmt (Seq stmts) 0) ++ "}"



showPrettyStmt :: Stmt
            -> Integer  -- ^ The precedence of the root expression, see the doc for 'HelpShow.parenthesize' for more detail
            -> String  -- ^ the minimally parenthesized string representing the input Ast
showPrettyStmt (Assignment str expr) i =  str ++ " := " ++ (showPrettyExpr expr 0)  ++ ";"
showPrettyStmt (If expr stmts) i = "if (" ++ (showPrettyExpr expr 0) ++ ") {" ++ (showPrettyStmt stmts 0) ++ "};" 

showPrettyStmt (While expr stmts) i = "while (" ++ (showPrettyExpr expr 0) ++ ") {" ++ (showPrettyStmt stmts 0)  ++ "};"
showPrettyStmt (Return expr) i = "return " ++ showPrettyExpr expr 0 ++ ";"
showPrettyStmt (Separator l r) i =  (showPrettyStmt l 1) ++ " ; " ++ (showPrettyStmt r 1) ++ ";"
showPrettyStmt (Seq lst) i =  case lst of 
                                   [x] -> showPrettyStmt x 1 
                                   (x:xs) -> (showPrettyStmt x 1) ++ (showPrettyStmt (Seq xs) 1)
                                   _ -> ""
                                   


showPrettyExpr :: Expr
            -> Integer  -- ^ The precedence of the root expression, see the doc for 'HelpShow.parenthesize' for more detail
            -> String  -- ^ the minimally parenthesized string representing the input Ast
showPrettyExpr (Var str) _ = str
showPrettyExpr (ValInt i) _ = if i < 0
                              then  "(" ++ show i ++ ")"
                              else show i
showPrettyExpr (ValBool True) i = "true"
showPrettyExpr (ValBool False) i = "false"
showPrettyExpr (Plus l r) i = parenthesize 10 i $ (showPrettyExpr l 10) ++ " + " ++ (showPrettyExpr r 11)
showPrettyExpr (Minus l r) i = parenthesize 10 i $ (showPrettyExpr l 10) ++ " - " ++ (showPrettyExpr r 11)
showPrettyExpr (Mult l r) i = parenthesize 12 i $ (showPrettyExpr l 12) ++ " * " ++ (showPrettyExpr r 13)
showPrettyExpr (Div l r) i = parenthesize 12 i $ (showPrettyExpr l 12) ++ " / " ++ (showPrettyExpr r 13)
showPrettyExpr (Mod l r) i = parenthesize 12 i $ (showPrettyExpr l 12) ++ " % " ++ (showPrettyExpr r 13)
showPrettyExpr (Or l r) i = parenthesize 6 i $ (showPrettyExpr l 6) ++ " || " ++ (showPrettyExpr r 7)
showPrettyExpr (And l r) i = parenthesize 8 i $ (showPrettyExpr l 8) ++ " && " ++ (showPrettyExpr r 9)
showPrettyExpr (Not expr) i = parenthesize 15 i $  " ! " ++ (showPrettyExpr expr 15)
showPrettyExpr (LessThan l r) i = parenthesize 9 i $ (showPrettyExpr l 9) ++ " < " ++ (showPrettyExpr r 10)
showPrettyExpr (GreaterThan l r) i = parenthesize 9 i $ (showPrettyExpr l 9) ++ " > " ++ (showPrettyExpr r 10)
showPrettyExpr (LessThanEq l r) i = parenthesize 9 i $ (showPrettyExpr l 9) ++ " <= " ++ (showPrettyExpr r 10)
showPrettyExpr (GreaterThanEq l r) i = parenthesize 9 i $ (showPrettyExpr l 9) ++ " >= " ++ (showPrettyExpr r 10)
showPrettyExpr (Eq l r) i = parenthesize 9 i $ (showPrettyExpr l 9) ++ " == " ++ (showPrettyExpr r 10)
showPrettyExpr (Neq l r) i = parenthesize 9 i $ (showPrettyExpr l 9) ++ " != " ++ (showPrettyExpr r 10)