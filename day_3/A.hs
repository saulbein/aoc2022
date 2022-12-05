import Data.Function
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.Environment

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (first, second) = (f first, f second)

main = do
    (inputPath:_) <- getArgs
    content <- readFile inputPath
    print (content & parseContents & (map calculatePriority) & sum)

type Item = Char
type Compartments = (Set.Set Item, Set.Set Item)
data Backpack = Backpack { size :: Int, items :: [Item] } deriving (Show)

parseContents :: String -> [Backpack]
parseContents contents = contents & lines & (map parseBackpack)
    where parseBackpack line = Backpack { size = length line, items = line }

calculatePriority :: Backpack -> Int
calculatePriority backpack = backpack & getCompartments & findDuplicate & getPriority

getCompartments :: Backpack -> Compartments
getCompartments backpack = splitAt halfSize (items backpack) & (mapTuple Set.fromList)
    where 
        halfSize
            | even (size backpack) = size backpack `div` 2 
            | otherwise            = error ("Could not get compartments for backpack " ++ 
                                            show backpack ++ 
                                            " of size " ++ 
                                            show (size backpack))

findDuplicate :: Compartments -> Item
findDuplicate (first, second) = (first `Set.intersection` second) & Set.toList & head

getPriority :: Item -> Int
getPriority item = case (Map.lookup item priorities) of 
    Just value -> value
    Nothing    -> error ("Could not find priority of item " ++ show item)
    where priorities = Map.fromList (zip (['a'..'z'] ++ ['A'..'Z']) [1..52])
