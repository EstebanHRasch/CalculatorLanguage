module Eval where

import Data.Map (Map)
import qualified Data.Map as Map

import Ast
import StateError
import Parser
import ParserMonad


-- the goal of the program is to return a value, what values are possible?
data Val = 
  I Integer | B Bool deriving Eq

instance Show Val where
  -- display the ast in a readable way
  show (I i) = show i
  show (B b) = show b
  
-- Val should handle Ints, Bools, and possibly lists, but not functions since the language is not functional

-- | helper function that runs with the default environment (for example, the stdLib in week 10)
-- return either the error string or the value, along with everything that was printed
run :: ModuleAst
      -> Either String (State, Val)
run a = runStateError (evalModule a) Map.empty 


type State = Map String Val


getVar :: String -> StateError String (Map String d) d
getVar v = do e <- getState
              case (Map.lookup v e) of
                Just i -> return i
                Nothing -> err "can't find value in env"

setVar :: String -> Expr -> StateError String State ()
setVar n v = do e <- getState
                v' <- evalExpr v
                putState (Map.insert n v' e)


-- every procedure call gets a new state
evalModule :: ModuleAst -> StateError String State Val
evalModule (ModuleAst s stmt) = evalStmt (Seq stmt) 

evalStmt :: Stmt -> StateError String State Val
evalStmt stmt = evalStmt' stmt (return (I 0))


evalStmt' :: Stmt -> (StateError String State Val) -> StateError String State Val
evalStmt' (Separator l r) next = evalStmt' l $ evalStmt' r next
evalStmt' (Return e) next = evalExpr e -- opt to skip the next stuff if you finish early
evalStmt' (Assignment n v) next = do
  setVar n v
  next
evalStmt' (Seq []) next = next
evalStmt' (Seq (s:ss)) next = evalStmt' s (evalStmt' (Seq ss) next)
evalStmt' (If e stm) next = do
  e' <- evalExpr e
  if e' == (B True)
     then evalStmt' stm next
     else next
evalStmt' (While e stm) next = do
  e' <- evalExpr e
  if e' == (B True)
     then evalStmt' (Seq [stm, While e stm]) next
     else next



-- basic way to eval 
{-
evalStmt :: Stmt -> EnvUnsafe Procs Env Expr
evalStmt (While x s) =  do x' <- evalExpr x
                           e <- getEnv
                           case x' of
                            Ok True -> runEnvUnsafe e 
                           return x'

evalStmt (Assignment s x) = do e <- getState
                            x' <- evalStmt (Return x)
                            case runEnvUnsafe (evalExpr bod) (Map.insert s x' e) of
                                 Ok a -> return a
                                 Error x -> err x
         
evalStmt (If i t e) = do i' <- evalBool i
                         t' <- evalStmt (Return t)
                         e' <- evalStmt (Return e)
                         if i' == True
                           then return t'
                           else return e'
         
evalStmt (Return x) = do x' <- evalExpr x
                         return x'
-}

{-
-- unfortunately we can't beak out on the first return!
exBad = runStateError (evalStmt $  Return (LitteralInt 2) `Seperator` Return (LitteralInt 7) ) Map.empty Map.empty
alternatively you may want to evaluate statements like this
evalStmt' :: Stmt -> (StateError String Procs State Val) -> StateError String Procs State Val
evalStmt' (Seperator l r) next = evalStmt' l $ evalStmt' r next
evalStmt' (Return e) next = evalExpr e -- opt to skip the next stuff if you finish early
evalStmt' (Assign n e) next = do
  e' <- evalExpr e
  setVar n e'
  next
...
it uses a simplified version of "continuation passing style", allows much finer control over control flow
evalStmt :: Stmt -> StateError String Procs State Val
evalStmt stmt = evalStmt' stmt (pure undefined) 
  
-- we can't beak out on the first return!
exGood = runEnvUnsafe (evalStmt  $ Return (ValInt 2) `Seperator` Return (ValInt 7) ) Map.empty Map.empty
exGood1 = runEnvUnsafe (evalStmt $ ("x" `Assign` (LitteralInt 2)) `Seperator` ("x" `Assign` (LitteralInt 7)) ) Map.empty Map.empty 
-}



-- 


evalExpr :: Expr -> StateError String State Val
evalExpr (ValInt i) = return $ I i
evalExpr (ValBool b) = return $ B b

evalExpr (Plus l r) = do l' <- evalInt l
                         r' <- evalInt r
                         return $ I $ l' + r'
evalExpr (Minus l r) = do l' <- evalInt l
                          r' <- evalInt r
                          return $ I $ l' - r'
evalExpr (Mult l r) = do l' <- evalInt l
                         r' <- evalInt r
                         return $ I $ l' * r'
evalExpr (Div l r) = do l' <- evalInt l
                        r' <- evalInt r
                        if r' == 0
                          then err "cannot divide by 0"
                          else return $ I $ l' `div`  r'
evalExpr (Mod l r) = do l' <- evalInt l
                        r' <- evalInt r
                        if r' == 0
                          then err "cannot mod by 0"
                          else return $ I $ l' `mod`  r'


evalExpr (And l r) = do l' <- evalBool l
                        r' <- evalBool r
                        return $ B $ l' && r'
evalExpr (Or l r) = do l' <- evalBool l
                       r' <- evalBool r
                       return $ B $ l' || r'
evalExpr (Not b) = do b' <- evalBool b
                      return $ B $ not b'

evalExpr (LessThan l r) = do l' <- evalInt l
                             r' <- evalInt r
                             return $ B $ l' < r'
evalExpr (GreaterThan l r) = do l' <- evalInt l
                                r' <- evalInt r
                                return $ B $ l' > r'
evalExpr (LessThanEq l r) = do l' <- evalInt l
                               r' <- evalInt r
                               return $ B $ l' <= r'
evalExpr (GreaterThanEq l r) = do l' <- evalInt l
                                  r' <- evalInt r
                                  return $ B $ l' >= r'
evalExpr (Eq l r) = do l' <- evalExpr l
                       r' <- evalExpr r
                       return $ B $ l' == r'
evalExpr (Neq l r) = do l' <- evalExpr l
                        r' <- evalExpr r
                        return $ B $ not (l' == r')

evalExpr (Var s) = do e <- getState
                      case Map.lookup s e of
                        Nothing -> err "can't find value in env"
                        Just a -> return a

evalInt :: Expr -> StateError String State Integer
evalInt a = do res <- evalExpr a
               case res of
                I i -> return i
                _   -> err "not the int we expected"


evalBool :: Expr -> StateError String State Bool
evalBool a = do res <- evalExpr a
                case res of
                 B b -> return b
                 _   -> err "not the bool we expected"

--exec :: Ast -> Either String Val
--exec a = runStateError (evalModule a) Map.empty
                     
execAst :: ModuleAst -> Either String (State, Val)
execAst a = runStateError (evalModule a) Map.empty

execParse :: String -> Either String (State, Val)
execParse s = case (parse parser s) of 
  Nothing -> Left "Parse Error"
  Just (a, "") -> execAst a
  Just (a, s) -> Left "Parse Error"

execParseEs :: String -> Either String (State, Val)
execParseEs s = case (parse parserExpr s) of 
  Nothing -> Left "Parse Error"
  Just (a, "") -> execEs a
  Just (a, s) -> Left "Parse Error" 

execEs :: Expr -> Either String (State, Val)
execEs a = runStateError (evalExpr a) Map.empty


execParseSts :: String -> Either String (State, Val)
execParseSts a = case (parse parserSeqStmts a) of 
  Nothing -> Left "Parse Error"
  Just (a, "") -> execSts a
  Just (a, s) -> Left "Parse Error" 

execSts :: Stmt -> Either String (State, Val)
execSts a = runStateError (evalStmt a) Map.empty
