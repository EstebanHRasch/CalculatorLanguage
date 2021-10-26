module Parser where

import Ast
import ParserMonad


-- | parser for the language
parser :: Parser ModuleAst
parser = parsemodule

parserSeqStmts = parseSeq

parserExpr = parseExp

{-
parse things like:
module name {
  def ...
  def ...
  def ...
}
-}
{-
parsemodule :: Parser ModuleAst
parsemodule =  do token $ literal "module"
                 name <- token varParser
                 token $ literal "{"
                 expr <- parseProcedureBod
                 token $ literal "}"
                 return evalModule expr -- check correctness
{-
parse things like:
def (x,y,z){
  ...
}
-}
-}
parsemodule :: Parser ModuleAst
parsemodule = do name <- token varParser
                 token $ literal "{"
                 stmts <- createSeqLst
                  --get list of statements
                 token $ literal "}"
                 return (ModuleAst name stmts)

{-
assignment , if , while , return 
parse things like:
x := ... ; retrun ...
or like:
while (...){
  ...
}
or like:
if(...){
...
}else{
...
}
-}

------------------------------------------------------------------------------
parseStmt :: Parser Stmt
parseStmt = ifParser <|> whileParser <|> retParser <|> asgnParser
{-
or you can try
parseStmts :: Parser [Stmt]
parseStmts = undefined
-}

parseSeq :: Parser Stmt
parseSeq = do stmts <- createSeqLst
              return $ Seq stmts

createSeqLst :: Parser [Stmt]
createSeqLst = do st <- parseStmt
                  (do rest <- createSeqLst
                      return $ [st] ++ rest) <|> return [st]
                  




asgnParser :: Parser Stmt
asgnParser = do var <- token $ varParser
                token $ literal ":="
                expr <- token $ parseExp
                token $ literal ";"
                return $ Assignment var expr

ifParser :: Parser Stmt
ifParser = do token $ literal "if"
              token $ literal "("
              expr <- parseExp
              token $ literal ")"
              token $ literal "{"
              x <- parseSeq
              token $ literal "}" 
              token $ literal ";"
              return $ If expr x 
              

whileParser :: Parser Stmt
whileParser = do token $ literal "while"
                 token $ literal "("
                 expr <- parseExp
                 token $ literal ")"
                 token $ literal "{"
                 x <- parseSeq
                 token $ literal "}"
                 token $ literal ";"
                 return $ While expr x 

retParser :: Parser Stmt
retParser = do token $ literal "return"
               expr <- parseExp
               token $ literal ";"
               return $ Return expr

---------------------------------------------------------------------------------------

{-
parse things like:
x+7%23 /4 == 3 || x != y && f(1,x,3+80) == true
-}
parseExp :: Parser Expr
parseExp =  orParse

----------------------------------------

orParse :: Parser Expr
orParse = withInfix andParse [("||", Or)]

andParse :: Parser Expr
andParse = withInfix parseEquality [("&&", And)]

----------------------------------------

parseEquality :: Parser Expr
parseEquality = withInfix parseNot [("<=", LessThanEq), (">=", GreaterThanEq), ("<", LessThan), (">", GreaterThan), ("==", Eq), ("!=", Neq)]

-----------------------------------------

parseNot :: Parser Expr
parseNot = (do token $ literal "!"
               expr <- parseNot
               return (Not expr)) <|> parsePlusSub

----------------------------------------

parsePlusSub :: Parser Expr
parsePlusSub = withInfix parseMultDiv [("+", Plus), ("-", Minus)]

parseMultDiv :: Parser Expr
parseMultDiv = withInfix parseAtoms [("*", Mult), ("/", Div), ("%", Mod)]

--------------------------------
parseAtoms :: Parser Expr
parseAtoms = ints <|> bools <|> parens <|> vars


keywords = ["if","then","else", "while", "let", "in", "true","false"]

vars :: Parser Expr
vars = do s <- token $ varParser
          if s `elem` keywords
          then failParse
          else return $ Var s


ints :: Parser Expr
ints = do i <- (token intParser)
          return $ ValInt i

bools :: Parser Expr
bools = do s <- token $ (literal "true") <||> (literal "false")
           case s of 
             Left _ -> return $ ValBool True
             Right _ -> return $ ValBool False


parens :: Parser Expr
parens = do token $ literal "("
            expr <- parseExp
            token $ literal ")"
            return expr

------------------------------------------



-- how you can set up a call experssion
procCall = do
  f <- token $ varParser
  (do token $ literal "("
      args <- withInfix (fmap (\x -> [x]) parseExp) [(",", (++))]
      token $ literal  ")"
      return  undefined) <|> undefined -- (return $ Var f )

