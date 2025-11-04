{-
  A basic calculator for arithmetic expressions
  Based on the example in Chapter 8 of "Programming in Haskell"
  by Graham Hutton.

  Pedro Vasconcelos, 2025
-}
module Main where

import Parsing 
import Data.Char

type Env = [(String, Integer)] -- var

findVar :: String -> Env -> Integer
findVar name [] = error ("Variable" ++ name ++ "not found.")
findVar name ((var, value):rest)
  | name == var = value
  | otherwise = findVar name rest

updateVar :: String -> Integer -> Env -> Env
updateVar name value [] = [(name, value)]
updateVar name value ((var,val):rest)
  | name == var = (var, value) : rest
  | otherwise = (var, val) : updateVar name value rest

--
-- a data type for expressions
-- made up from integer numbers, + and *
--
data Expr = Num Integer
          | Var String
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr 
          | Mod Expr Expr
          deriving Show

data Command = Assign String Expr -- atribuição
              | Eval Expr         -- expressão
              deriving Show

-- a recursive evaluator for expressions
--
eval :: Env -> Expr -> Integer
eval  _ (Num n) = n
eval env (Var n) = findVar n env
eval env (Add e1 e2) = eval env e1 + eval env e2
eval env (Sub e1 e2) = eval env e1 - eval env e2
eval env (Mul e1 e2) = eval env e1 * eval env e2
eval env (Div e1 e2) = eval env e1 `div` eval env e2
eval env (Mod e1 e2) = eval env e1 `mod` eval env e2

-- | a parser for expressions
-- Grammar rules:
--
-- expr ::= term exprCont
-- exprCont ::= '+' term exprCont | epsilon

-- term ::= factor termCont
-- termCont ::= '*' factor termCont | epsilon

-- factor ::= natural | '(' expr ')'
-- factor ::= variable | natural | '(' expr ')' (Part2)

-- command ::= variable '=' expr | expr (Part2)

expr :: Parser Expr
expr = do t <- term
          exprCont t

exprCont :: Expr -> Parser Expr
exprCont acc = do char '+'
                  t <- term
                  exprCont (Add acc t)
               <|>
               do char '-'
                  t <- term
                  exprCont (Sub acc t)
               <|> return acc
              
term :: Parser Expr
term = do f <- factor
          termCont f

termCont :: Expr -> Parser Expr
termCont acc =  do char '*'
                   f <- factor  
                   termCont (Mul acc f)
                 <|>
                do char '/'
                   f <- factor
                   termCont (Div acc f)
                 <|>
                do char '%'
                   f <- factor 
                   termCont (Mod acc f)
                 <|> return acc

factor :: Parser Expr
factor = do n <- natural
            return (Num n)
          <|>
          do v <- variable
             return (Var v)
          <|>
          do char '('
             e <- expr
             char ')'
             return e
             
command :: Parser Command
command = do v <- variable
             char '='
             e <- expr
             return (Assign v e)
          <|>
          do e <- expr
             return (Eval e)    
             

natural :: Parser Integer
natural = do xs <- many1 (satisfy isDigit)
             return (read xs)

variable :: Parser String
variable = do xs <- many1 (satisfy isAlpha)
              return xs

----------------------------------------------------------------             
  
main :: IO ()
main = do
  txt <- getContents
  calculator [] (lines txt)


-- | read-eval-print loop
calculator :: Env -> [String] -> IO ()
calculator _ []            = return ()
calculator env (l:ls) = do
  let (out, env') = evaluate env l
  putStrLn out
  calculator env' ls


-- | evaluate a single expression
evaluate :: Env -> String -> (String, Env)
evaluate env txt =
  case parse command txt of
    [(Assign var expression, "")] -> -- avalia, produz, output
      let val  = eval env expression
          env' = updateVar var val env
      in (show val, env')
    [(Eval expression, "")] ->      -- só output
      (show (eval env expression), env)
    _ -> ("parse error; try again", env)
