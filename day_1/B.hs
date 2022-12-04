import Data.List

main = do
    contents <- readFile "input.txt"
    print (sum . findTop3 . parseContents $ contents)

parseContents :: String -> [[Integer]]
parseContents = ((map . map) read) . (split "") . (split '\n')

findTop3 :: [[Integer]] -> [Integer]
findTop3 = (take 3) . (sortBy (flip compare)) . (map sum)

split :: (Eq a) => a -> [a] -> [[a]]
split _ [] = []
split delim [first] 
    | first == delim = [] 
    | otherwise      = [[first]]
split delim (first:remaining)
    | first == delim       = [] : split delim remaining
    | remaining == [delim] = [[first]]
    | otherwise            = (first:rest_of_cell) : others
        where (rest_of_cell:others) = split delim remaining

