import Data.Function
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.Environment

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk len lst
    | len <= 0          = error "Can only chunk to positive-length chunks"
    | otherwise         = firstChunk : chunk len rest
        where (firstChunk, rest) = splitAt len lst

main = do
    (inputPath:_) <- getArgs
    content <- readFile inputPath
    print (content & (parseContents 3) & (map calculatePriority) & sum)

type Item = Char
type Backpack = [Item]
type Group = [Backpack]

parseContents :: Int -> String -> [Group]
parseContents chunkSize contents = contents & lines & chunk chunkSize 

calculatePriority :: Group -> Int
calculatePriority group = group & findDuplicate & getPriority

findDuplicate :: Group -> Item
findDuplicate group = group & (map Set.fromList) & (foldl1 Set.intersection) & Set.toList & head

getPriority :: Item -> Int
getPriority item = case (Map.lookup item priorities) of 
    Just value -> value
    Nothing    -> error ("Could not find priority of item " ++ show item)
    where priorities = Map.fromList (zip (['a'..'z'] ++ ['A'..'Z']) [1..52])
