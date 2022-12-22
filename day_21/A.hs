#!/usr/bin/env stack
{- stack script
 --resolver lts-20.3
 --install-ghc
-}
import Data.Function
import qualified Data.List as List
import qualified Data.Map as Map
import System.Environment

main = do
  (inputPath:_) <- getArgs
  contents <- readFile inputPath
  let expressions = contents & parseExpressions
  print (resolve "root" expressions)

type Variable = String

data Expression
  = Value Int
  | Add Variable Variable
  | Subtract Variable Variable
  | Multiply Variable Variable
  | Divide Variable Variable
  deriving (Show)

parseExpressions :: String -> Map.Map Variable Expression
parseExpressions content = content & lines & map parseExpression & Map.fromList

parseExpression :: String -> (Variable, Expression)
parseExpression s =
  let (name, _:_:rest) = break (== ':') s
   in (name, parseOp rest)

parseOp :: String -> Expression
parseOp s =
  if null rest
    then Value (read s :: Int)
    else let (_:op:_:var2) = rest
          in case op of
               '+' -> Add var1 var2
               '-' -> Subtract var1 var2
               '*' -> Multiply var1 var2
               '/' -> Divide var1 var2
  where
    (var1, rest) = break (== ' ') s

resolve :: Variable -> Map.Map Variable Expression -> Int
resolve name expressions =
  case Map.lookup name expressions of
    Nothing -> error ("Unknown variable: " ++ name)
    Just expression ->
      case expression of
        Value x -> x
        Add x y -> resolve x expressions + resolve y expressions
        Subtract x y -> resolve x expressions - resolve y expressions
        Multiply x y -> resolve x expressions * resolve y expressions
        Divide x y -> resolve x expressions `div` resolve y expressions
