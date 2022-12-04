import Data.List
import Data.Function

main = do
    contents <- readFile "input.txt"
    print (contents & parseContents & (map getResult) & (map getScore) & sum)


data Shape = Rock | Paper | Scissors deriving (Enum, Bounded, Eq, Show)
data State = Loss | Draw | Win deriving (Enum, Show)
data Play = Play { oponent :: Shape, neededResult :: State } deriving (Show)
data Result = Result { state :: State, shape :: Shape } deriving (Show)

parseContents :: String -> [Play]
parseContents contents = contents & lines & map parseLine
    where parseLine line = line & words & toPlay
          toPlay (first:second:[]) = Play { oponent = parseShape first, 
                                            neededResult = parseResult second }
          parseShape text = case text of "A" -> Rock
                                         "B" -> Paper
                                         "C" -> Scissors
                                         otherwise -> error ("Unexpected shape: " ++ text)
          parseResult text = case text of "X" -> Loss
                                          "Y" -> Draw
                                          "Z" -> Win
                                          otherwise -> error ("Unexpected state: " ++ text)

getResult :: Play -> Result
getResult play = Result { state = neededResult play, shape = deduceMyShape play }
    where
    next :: (Bounded a, Enum a, Eq a) => a -> a
    next x = if x == maxBound then minBound else succ x
    prev :: (Bounded a, Enum a, Eq a) => a -> a
    prev x = if x == minBound then maxBound else pred x
    deduceMyShape play = case neededResult play of Loss -> prev (oponent play)
                                                   Draw -> oponent play
                                                   Win  -> next (oponent play)

getScore :: Result -> Int
getScore result = 1 + fromEnum (shape result) + case state result of Loss -> 0
                                                                     Draw -> 3
                                                                     Win  -> 6
