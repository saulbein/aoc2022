import Data.Function
import System.Environment
import qualified Data.Text as Text

toPair :: Show a => [a] -> (a, a)
toPair (first:second:[]) = (first, second)
toPair otherList = error ("Could not convert list " ++ show otherList ++ " to a pair")

main = do
    (inputPath:_) <- getArgs
    contents <- readFile inputPath
    print (contents & parseContents & (filter isOverlapping) & length)

type Range = (Int, Int)
type PairAssignments = (Range, Range)

parseContents :: String -> [PairAssignments]
parseContents contents = contents & lines & (map parseAssignments)
    where 
    parseAssignments line = line & 
        Text.pack & 
        (Text.splitOn (Text.singleton ',')) & 
        (map parseRange) & 
        toPair 
    parseRange text = text & 
        (Text.splitOn (Text.singleton '-')) & 
        (map Text.unpack) & 
        (map read) & 
        toPair

isOverlapping :: PairAssignments -> Bool
isOverlapping (r1@(start1, end1), r2@(start2, end2)) = isInside start1 r2 || 
                                                       isInside end1 r2 ||
                                                       isInside start2 r1 ||
                                                       isInside end2 r1
    where isInside point (start, end) = point >= start && point <= end
