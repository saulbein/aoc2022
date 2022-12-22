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
  let expressions = parseExpressions contents
      tree = buildExprTree "root" expressions
      (UnresolvedVar, answer) = resolveUnknown (tree, 0)
  print expressions
  print tree
  print answer

type Variable = String

data Op
  = Plus
  | Minus
  | Star
  | Slash
  deriving (Show)

data Expression
  = Human
  | Value Int
  | TwoArgOp Variable Variable Op
  | Root Variable Variable
  deriving (Show)

parseExpressions :: String -> Map.Map Variable Expression
parseExpressions content = content & lines & map parseExpression & Map.fromList

parseExpression :: String -> (Variable, Expression)
parseExpression s =
  let (name, _:_:rest) = break (== ':') s
   in (name, parseOp name rest)

parseOp :: Variable -> String -> Expression
parseOp name s
  | name == "humn" = Human
  | name == "root" =
    let (TwoArgOp var1 var2 _) = parseWithArgs rest
     in Root var1 var2
  | null rest = Value (read s :: Int)
  | otherwise = parseWithArgs rest
  where
    (var1, rest) = break (== ' ') s
    parseWithArgs s =
      let (_:op:_:var2) = s
       in TwoArgOp var1 var2 (parseMath op)
    parseMath c =
      case c of
        '+' -> Plus
        '-' -> Minus
        '*' -> Star
        '/' -> Slash

data Expr
  = UnresolvedVar
  | UnresolvedLeft Expr
  | UnresolvedRight Expr
  | Val Int
  | Eq Expr Expr
  | Add Expr Expr
  | Subtract Expr Expr
  | Multiply Expr Expr
  | Divide Expr Expr
  deriving (Show)

buildExprTree :: Variable -> Map.Map Variable Expression -> Expr
buildExprTree name expressions =
  case Map.lookup name expressions of
    Nothing -> error ("Unknown variable: " ++ name)
    Just expression ->
      case expression of
        Human -> UnresolvedVar
        Value x -> Val x
        Root x y ->
          Eq (buildExprTree x expressions) (buildExprTree y expressions)
        TwoArgOp x y op ->
          case (buildExprTree x expressions, buildExprTree y expressions) of
            (e@UnresolvedVar, resolved) -> UnresolvedLeft (toExpr op e resolved)
            (e@(UnresolvedLeft {}), resolved) ->
              UnresolvedLeft (toExpr op e resolved)
            (e@(UnresolvedRight {}), resolved) ->
              UnresolvedLeft (toExpr op e resolved)
            (resolved, e@UnresolvedVar) ->
              UnresolvedRight (toExpr op resolved e)
            (resolved, e@(UnresolvedLeft {})) ->
              UnresolvedRight (toExpr op resolved e)
            (resolved, e@(UnresolvedRight {})) ->
              UnresolvedRight (toExpr op resolved e)
            (r1, r2) -> toExpr op r1 r2

toExpr :: Op -> Expr -> Expr -> Expr
toExpr op l r =
  case op of
    Plus -> Add l r
    Minus -> Subtract l r
    Star -> Multiply l r
    Slash -> Divide l r

resolveKnown :: Expr -> Int
resolveKnown ex =
  case ex of
    Val x -> x
    Add x y -> resolveKnown x + resolveKnown y
    Subtract x y -> resolveKnown x - resolveKnown y
    Multiply x y -> resolveKnown x * resolveKnown y
    Divide x y -> resolveKnown x `div` resolveKnown y

type Equivalence = (Expr, Int)

resolveUnknown :: Equivalence -> Equivalence
resolveUnknown (ex, val) =
  case ex of
    UnresolvedVar -> (UnresolvedVar, val)
    UnresolvedLeft ex' ->
      case ex' of
        Add l r -> resolveUnknown (l, val - resolveKnown r)
        Subtract l r -> resolveUnknown (l, val + resolveKnown r)
        Multiply l r -> resolveUnknown (l, val `div` resolveKnown r)
        Divide l r -> resolveUnknown (l, val * resolveKnown r)
    UnresolvedRight ex' ->
      case ex' of
        Add l r -> resolveUnknown (r, val - resolveKnown l)
        Subtract l r -> resolveUnknown (r, resolveKnown l - val)
        Multiply l r -> resolveUnknown (r, val `div` resolveKnown l)
        Divide l r -> resolveUnknown (r, resolveKnown l `div` val)
    Eq UnresolvedVar l -> (UnresolvedVar, resolveKnown l)
    Eq r UnresolvedVar -> (UnresolvedVar, resolveKnown r)
    Eq e@(UnresolvedLeft {}) l -> resolveUnknown (e, resolveKnown l)
    Eq r e@(UnresolvedLeft {}) -> resolveUnknown (e, resolveKnown r)
    Eq e@(UnresolvedRight {}) l -> resolveUnknown (e, resolveKnown l)
    Eq r e@(UnresolvedRight {}) -> resolveUnknown (e, resolveKnown r)
