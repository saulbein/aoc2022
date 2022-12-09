#!/usr/bin/env stack
{- stack script
 --resolver lts-20.3
 --install-ghc
-}
import Data.Function
import qualified Data.List as List
import System.Environment

main = do
  (inputPath:_) <- getArgs
  contents <- readFile inputPath
  let origin = Position {x = 0, y = 0}
      ropeLength = 10
      initialState =
        State {positions = replicate ropeLength origin, tailVisited = []}
  print
    (contents & parseContent & foldl applyMovement initialState & tailVisited &
     List.nub &
     length)

data MovementDirection
  = U
  | D
  | L
  | R
  deriving (Show)

type Movement = (MovementDirection, Int)

parseContent :: String -> [Movement]
parseContent content = content & lines & map parseDirection

parseDirection :: String -> Movement
parseDirection line =
  ( case direction of
      "U" -> U
      "D" -> D
      "L" -> L
      "R" -> R
  , distance)
  where
    (direction, _:distStr) = break (== ' ') line
    distance = read distStr :: Int

data Position =
  Position
    { x :: Int
    , y :: Int
    }
  deriving (Show, Eq)

data State =
  State
    { positions :: [Position]
    , tailVisited :: [Position]
    }
  deriving (Show)

data RelativeDirection
  = Same
  | ToTop
  | ToTopRight
  | ToRight
  | ToBottomRight
  | ToBottom
  | ToBottomLeft
  | ToLeft
  | ToTopLeft
  deriving (Show, Eq)

distance :: Position -> Position -> Int
distance p1 p2 = max (abs (x p1 - x p2)) (abs (y p1 - y p2))

relativeDirection :: Position -> Position -> RelativeDirection
relativeDirection from to
  | from == to = Same
  | x from == x to && y from < y to = ToTop
  | x from < x to && y from < y to = ToTopRight
  | x from < x to && y from == y to = ToRight
  | x from < x to && y from > y to = ToBottomRight
  | x from == x to && y from > y to = ToBottom
  | x from > x to && y from > y to = ToBottomLeft
  | x from > x to && y from == y to = ToLeft
  | x from > x to && y from < y to = ToTopLeft

moveTowards :: RelativeDirection -> Position -> Position
moveTowards towards from =
  case towards of
    Same -> from
    ToTop -> Position {x = x from, y = y from + 1}
    ToTopRight -> Position {x = x from + 1, y = y from + 1}
    ToRight -> Position {x = x from + 1, y = y from}
    ToBottomRight -> Position {x = x from + 1, y = y from - 1}
    ToBottom -> Position {x = x from, y = y from - 1}
    ToBottomLeft -> Position {x = x from - 1, y = y from - 1}
    ToLeft -> Position {x = x from - 1, y = y from}
    ToTopLeft -> Position {x = x from - 1, y = y from + 1}

followTo :: Position -> Position -> Position
followTo tp hp
  | distance hp tp <= 1 = tp
  | otherwise = moveTowards (relativeDirection tp hp) tp

applyMovement :: State -> Movement -> State
applyMovement state (dir, dist) = iterate (singleMove dir) state !! dist

singleMove :: MovementDirection -> State -> State
singleMove move state =
  State {positions = newPositions, tailVisited = newTailPos : tailVisited state}
  where
    direction =
      case move of
        U -> ToTop
        D -> ToBottom
        L -> ToLeft
        R -> ToRight
    (oldHead:others) = positions state
    newHeadPos = moveTowards direction oldHead
    newPositions = followHead newHeadPos others
    newTailPos = last newPositions
    followHead :: Position -> [Position] -> [Position]
    followHead headPos [] = [headPos]
    followHead headPos (nextPos:ps) = headPos : followHead newNextPos ps
      where
        newNextPos = followTo nextPos headPos
