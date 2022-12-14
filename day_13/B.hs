#!/usr/bin/env stack
{- stack script
 --resolver lts-20.3
 --install-ghc
-}
import Control.Arrow
import Data.Function
import qualified Data.List as List
import Data.Maybe
import System.Environment

main = do
  (inputPath:_) <- getArgs
  contents <- readFile inputPath
  let parsed = contents & parseContent
      dividers = [List [List [Number 2]], List [List [Number 6]]]
      result =
        parsed ++ dividers & List.sort & zip [1 ..] &
        filter (\(_, item) -> item `elem` dividers) &
        map fst &
        product
  print result

data Packet
  = Number Int
  | List [Packet]
  deriving (Eq)

data Token
  = ListStart
  | ListEnd
  | ListSeparator
  | Value String
  deriving (Show)

instance Show Packet where
  show (Number n) = show n
  show (List l) = show l

parseContent :: String -> [Packet]
parseContent content = content & lines & filter (/= "") & map parseLine

parseLine :: String -> Packet
parseLine line = tokenize line & parseList & fromJust & snd

tokenize :: String -> [Token]
tokenize s = go s ""
  where
    go :: String -> String -> [Token]
    go "" "" = []
    go "" valueAcc = [Value valueAcc]
    go (c:rest) "" =
      case c of
        '[' -> ListStart : go rest ""
        ']' -> ListEnd : go rest ""
        ',' -> ListSeparator : go rest ""
        c -> go rest [c]
    go (c:rest) valueAcc
      -- there's probably a way to join these branches but I'm lazy
     =
      case c of
        '[' -> Value valueAcc : ListStart : go rest ""
        ']' -> Value valueAcc : ListEnd : go rest ""
        ',' -> Value valueAcc : ListSeparator : go rest ""
        c -> go rest (valueAcc ++ [c])

parseValue :: [Token] -> Maybe ([Token], Packet)
parseValue (Value x:tokens) = Just (tokens, Number (read x :: Int))
parseValue _ = Nothing

parseList :: [Token] -> Maybe ([Token], Packet)
parseList (ListStart:tokens) =
  case parseListContents tokens [] of
    Just (moreTokens, packets) ->
      case moreTokens of
        (ListEnd:evenMoreTokens) -> Just (evenMoreTokens, List packets)
        _ -> Nothing
    _ -> Nothing
parseList _ = Nothing

parseListContents :: [Token] -> [Packet] -> Maybe ([Token], [Packet])
parseListContents tokens@(ListEnd:_) [] = Just (tokens, [])
parseListContents tokens [] =
  case parseListItem tokens of
    Just (moreTokens, item) -> parseListContents moreTokens [item]
    _ -> Nothing
parseListContents tokens packets =
  case parseListItem tokens of
    Just (moreTokens, item) -> parseListContents moreTokens (packets ++ [item])
    _ -> Just (tokens, packets)

parseListItem :: [Token] -> Maybe ([Token], Packet)
parseListItem (ListSeparator:tokens) = parseListItem tokens
parseListItem tokens
  | isJust maybeValue = maybeValue
  | isJust maybeList = maybeList
  | otherwise = Nothing
  where
    maybeValue = parseValue tokens
    maybeList = parseList tokens

instance Ord Packet where
  compare (Number x) (Number y) = compare x y
  compare (List []) (List []) = EQ
  compare (List []) (List _) = LT
  compare (List _) (List []) = GT
  compare (List x) y@(Number _) = compare x [y]
  compare x@(Number _) (List y) = compare [x] y
  compare (List x) (List y) = compare x y
