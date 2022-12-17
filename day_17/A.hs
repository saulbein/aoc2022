#!/usr/bin/env stack
{- stack script
 --resolver lts-20.3
 --install-ghc
-}
import Control.Arrow
import Data.Function
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe
import System.Environment

blockTemplates =
  [ [(1, 1), (2, 1), (3, 1), (4, 1)] -- horizontal line
  , [(2, 1), (1, 2), (2, 2), (3, 2), (2, 3)] -- plus
  , [(1, 1), (2, 1), (3, 1), (3, 2), (3, 3)] -- backwards L
  , [(1, 1), (1, 2), (1, 3), (1, 4)] -- vertical line
  , [(1, 1), (2, 1), (1, 2), (2, 2)] -- square
  ]

loadInput inputPath = do
  contents <- readFile inputPath
  let jets = parseJets contents & cycle
      blocks = blockTemplates & map (map (uncurry Point)) & cycle
  return (jets, blocks)

main = do
  (inputPath:iter:_) <- getArgs
  contents <- readFile inputPath
  let jets = parseJets contents & cycle
      blocks = blockTemplates & map (map (uncurry Point)) & cycle
      initialState =
        GameState
          { board = Board {minX = 1, maxX = 7, minY = 1}
          , blocks = blocks
          , jets = jets
          , occupied = []
          , falling = []
          , height = 0
          }
      iterations = read iter :: Int
      finalState = iterate dropBlock initialState & take (iterations + 1) & last
  print (height finalState)

data Point =
  Point
    { x :: Int
    , y :: Int
    }
  deriving (Show, Eq)

type Block = [Point]

data Direction
  = ToDown
  | ToLeft
  | ToRight
  deriving (Show)

data Board =
  Board
    { minX :: Int
    , maxX :: Int
    , minY :: Int
    }
  deriving (Show)

data GameState =
  GameState
    { board :: Board
    , blocks :: [Block]
    , jets :: [Direction]
    , occupied :: [Point]
    , falling :: Block
    , height :: Int
    }

instance Show GameState where
  show state =
    "{GameState: \n" ++
    show (board state) ++
    "\n" ++
    show (occupied state) ++
    "\n" ++ show (falling state) ++ "\n" ++ show (height state) ++ "\n}"

parseJets :: String -> [Direction]
parseJets content = lines content & head & map parseDirection
  where
    parseDirection c =
      case c of
        '>' -> ToRight
        '<' -> ToLeft
        _ -> error "Unknown direction encountered"

dropBlock :: GameState -> GameState
dropBlock state =
  GameState
    { board = board state
    , blocks = other
    , jets = jets state
    , occupied = occupied state
    , falling = placeNewBlock (height state) new
    , height = height state
    } &
  iterate step &
  dropWhile (not . null . falling) &
  head
  where
    (new:other) = blocks state

step :: GameState -> GameState
step state =
  case fall (board state) (occupied state) pushed of
    Just fallen ->
      GameState
        { board = board state
        , blocks = blocks state
        , jets = otherJets
        , occupied = occupied state
        , falling = fallen
        , height = height state
        }
    Nothing ->
      GameState
        { board = board state
        , blocks = blocks state
        , jets = otherJets
        , occupied = occupied state ++ pushed
        , falling = []
        , height = max (height state) stopHeight
        }
  where
    js@(nextJet:otherJets) = jets state
    pushed = applyJet (board state) nextJet (occupied state) (falling state)
    stopHeight = pushed & map y & maximum

fall :: Board -> [Point] -> Block -> Maybe Block
fall board occupied block =
  if wouldCollide
    then Nothing
    else Just fallen
  where
    fallen = map (push ToDown) block
    wouldCollide = any (collides board occupied) fallen

applyJet :: Board -> Direction -> [Point] -> Block -> Block
applyJet board direction occupied block =
  if wouldCollide
    then block
    else pushed
  where
    pushed = map (push direction) block
    wouldCollide = any (collides board occupied) pushed

collides :: Board -> [Point] -> Point -> Bool
collides board occupied p = not (inBoard board p) || p `List.elem` occupied

push :: Direction -> Point -> Point
push direction p =
  case direction of
    ToDown -> move p (0, -1)
    ToLeft -> move p (-1, 0)
    ToRight -> move p (1, 0)

move :: Point -> (Int, Int) -> Point
move p (relX, relY) = Point {x = x p + relX, y = y p + relY}

inBoard :: Board -> Point -> Bool
inBoard b p = x p >= minX b && x p <= maxX b && y p >= minY b

placeNewBlock :: Int -> Block -> Block
placeNewBlock height = map (\p -> move p (2, height + 3))
