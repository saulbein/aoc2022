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
  let initialRegValue = 1 :: Int
      relevantStates = [20,60 .. 220] :: [Int]
      result =
        contents & parseContent &
        fmap (calculate initialRegValue relevantStates)
        where
          calculate initial relevant instructions =
            applyInstructions initial instructions &
            -- we need to get entries one earlier since we store the values after tick
            -- but the question wants values during (before) instruction is applied
            getSpecificIndices (map (subtract 1) relevant) &
            zipWith (*) relevantStates &
            sum
  case result of
    Left error -> print $ "Error: " ++ error
    Right result -> print result

data Instruction
  = Noop
  | Addx Int
  deriving (Show)

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

applyInstruction :: Int -> Instruction -> Seq.Seq Int
applyInstruction startValue instruction =
  case instruction of
    Noop -> Seq.singleton startValue
    Addx value -> Seq.fromList [startValue, startValue + value]

applyInstructions :: Int -> [Instruction] -> [Int]
applyInstructions value [] = []
applyInstructions value (headIns:instructions) =
  toList newValues ++ applyInstructions lastValue instructions
  where
    newValues@(_ Seq.:|> lastValue) = applyInstruction value headIns

getSpecificIndices :: [Int] -> [a] -> [a]
getSpecificIndices _ [] = []
getSpecificIndices indices list = dropToOffsets offsets list
  where
    offsets = zipWith (-) indices (1 : indices)
    dropToOffsets (offset:otherOffsets) list =
      case drop offset list of
        items@(relevantItem:otherItems) ->
          relevantItem : dropToOffsets otherOffsets items
        _ -> []
    dropToOffsets [] _ = []
