#!/usr/bin/env stack
{- stack script
 --resolver lts-20.3
 --package "containers"
 --install-ghc
-}
import Control.Monad
import Data.Foldable
import Data.Function
import qualified Data.List as List
import Data.Maybe
import qualified Data.Sequence as Seq
import System.Environment

main = do
  (inputPath:_) <- getArgs
  contents <- readFile inputPath
  let initialRegValue = 1
      monitorDimensions = MonitorDimensions {width = 40, height = 6}
      linePositions =
        [0 .. (width monitorDimensions * height monitorDimensions - 1)] :: [LinePos]
      result = contents & parseContent & fmap (calculate initialRegValue)
        where
          calculate initial instructions =
            applyInstructions initial instructions & (initial :) &
            zipWith (wherePaint monitorDimensions) linePositions &
            getLines &
            List.intercalate "\n" &
            (++ "\n")
  case result of
    Left error -> putStrLn $ "Error: " ++ error
    Right result -> putStr result

data Instruction
  = Noop
  | Addx Int
  deriving (Show)

data Point =
  Point
    { x :: Int
    , y :: Int
    , c :: Char
    }
  deriving (Show)

data MonitorDimensions =
  MonitorDimensions
    { width :: Int
    , height :: Int
    }
  deriving (Show)

type LinePos = Int

type ErrorMessage = String

parseContent :: String -> Either ErrorMessage [Instruction]
parseContent content = content & lines & mapM parseInstruction

parseInstruction :: String -> Either ErrorMessage Instruction
parseInstruction line
  | isJust maybeNoop = Right Noop
  | isJust maybeAddx = Right $ Addx (read (fromJust maybeAddx) :: Int)
  | otherwise = Left $ "Unknown instruction " ++ line
  where
    maybeAddx = List.stripPrefix "addx " line
    maybeNoop = List.stripPrefix "noop" line

applyInstruction :: LinePos -> Instruction -> Seq.Seq LinePos
applyInstruction startValue instruction =
  case instruction of
    Noop -> Seq.singleton startValue
    Addx value -> Seq.fromList [startValue, startValue + value]

applyInstructions :: LinePos -> [Instruction] -> [LinePos]
applyInstructions value [] = []
applyInstructions value (headIns:instructions) =
  toList newValues ++ applyInstructions lastValue instructions
  where
    newValues@(_ Seq.:|> lastValue) = applyInstruction value headIns

isClose :: LinePos -> LinePos -> Bool
isClose p1 p2 = abs (p1 - p2) <= 1

toCoords :: MonitorDimensions -> LinePos -> (Int, Int)
toCoords dimensions p = (x, y)
  where
    (y, x) = p `divMod` width dimensions

wherePaint :: MonitorDimensions -> LinePos -> LinePos -> Point
wherePaint dimensions monPos regPos = Point {x = x, y = y, c = c}
  where
    (x, y) = toCoords dimensions monPos
    c =
      if isClose x regPos
        then '#'
        else '.'

getLines :: [Point] -> [String]
getLines points = points & List.groupBy ((==) `on` y) & map (map c)
