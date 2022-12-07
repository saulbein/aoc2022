#!/usr/bin/env stack
{- stack script
 --resolver lts-20.3
 --install-ghc
 --package "containers regex-pcre-builtin text"
-}

import Data.Function
import qualified Data.List as List
import qualified Data.Map as Map
import System.Environment
import Text.Regex.PCRE

getSpecificIndices :: [Int] -> [a] -> [a]
getSpecificIndices _ [] = []
getSpecificIndices indices list = dropToOffsets offsets list 
    where 
    offsets = zipWith (-) indices (0:indices)
    dropToOffsets (offset:otherOffsets) list = case drop offset list of 
        items@(relevantItem:otherItems) -> relevantItem : dropToOffsets otherOffsets items
        _ -> []
    dropToOffsets [] _ = []

main = do
    (inputPath:_) <- getArgs
    contents <- readFile inputPath
    print (contents & 
        parseContents & 
        (\(cargo, instructions) -> applyInstructions cargo instructions) &
        getTopCrates)

type Crate = Char
type StackNo = Int
type Stack = [Crate]
type Cargo = Map.Map StackNo Stack
data Instruction = Instruction { 
    moveAmount :: Int, 
    startingStack :: StackNo, 
    endingStack :: StackNo }
    deriving (Show)

parseContents :: String -> (Cargo, [Instruction])
parseContents contents = (parseCargo cargoLines stackCount, map parseInstruction instructionSection)
    where
    (cargoLines, stackCount, instructionSection) = splitSections contents

splitSections :: String -> ([String], Int, [String])
splitSections contents = (init cargoSection, 
                          cargoSection & 
                            last & 
                            filter (/=' ') & 
                            last & 
                            (:[]) & 
                            (\s -> read s :: Int),
                          instructionsSection)
    where 
    (cargoSection, instructionsSection) = contents & lines & span (/="") & (\(c, i) -> (c, tail i))

parseCargo :: [String] -> Int -> Cargo
parseCargo cargo stackCount = cargo &
    reverse &
    (map getRowItems) & 
    List.transpose & 
    (map (takeWhile (/= ' '))) &
    zip [1..] &
    Map.fromList
    where 
    getRowItems = getSpecificIndices ([1,5..(stackCount * 4)])

parseInstruction :: String -> Instruction
parseInstruction line = line & 
    (\l -> l =~ "move ([0-9]+) from ([0-9]+) to ([0-9]+)" :: (String, String, String, [String])) &
    (\(_,_,_,matches) -> matches) &
    map (\s -> read s :: Int) &
    (\(move:from:to:[]) -> Instruction { moveAmount = move, startingStack = from, endingStack = to })

applyInstructions :: Cargo -> [Instruction] -> Cargo
applyInstructions = foldl applyInstruction

applyInstruction :: Cargo -> Instruction -> Cargo
applyInstruction cargo ins = cargo & 
    Map.insert (startingStack ins) toLeave & 
    Map.insert (endingStack ins) toPut
    where
    toPut = stackTo ++ reverse toGrab
    (toLeave, toGrab) = splitAt splitLoc stackFrom
    stackFrom = cargo Map.! (startingStack ins)
    stackTo = cargo Map.! (endingStack ins)
    splitLoc = length stackFrom - (moveAmount ins)

getTopCrates :: Cargo -> [Crate]
getTopCrates cargo = cargo & Map.elems & (map last)
