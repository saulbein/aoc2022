import Data.List
import Data.Function

main = do
    contents <- readFile "input.txt"
    print (contents & parseContents & (map getResult) & (map getScore) & sum)


data Shape = Rock | Paper | Scissors deriving (Enum, Bounded, Eq, Show)
data State = Loss | Draw | Win deriving (Enum, Show)
data Play = Play { oponent :: Shape, mine :: Shape } deriving (Show)
data Result = Result { state :: State, shape :: Shape } deriving (Show)

parseContents :: String -> [Play]
parseContents contents = contents & lines & map parseLine
    where parseLine line = line & words & toPlay
          toPlay (first:second:[]) = Play { oponent = oponentsShape first, mine = myShape second }
          oponentsShape text = case text of "A" -> Rock
                                            "B" -> Paper
                                            "C" -> Scissors
                                            otherwise -> error ("Unexpected shape: " ++ text)
          myShape text = case text of "X" -> Rock
                                      "Y" -> Paper
                                      "Z" -> Scissors
                                      otherwise -> error ("Unexpected shape: " ++ text)

getResult :: Play -> Result
getResult play = Result { state = rps play, shape = mine play }
    where
    next :: (Bounded a, Enum a, Eq a) => a -> a
    next x = if x == maxBound then minBound else succ x
    prev :: (Bounded a, Enum a, Eq a) => a -> a
    prev x = if x == minBound then maxBound else pred x
    rps play 
        | oponent play == next (mine play) = Loss 
        | oponent play == prev (mine play) = Win 
        | otherwise                        = Draw 

getScore :: Result -> Int
getScore result = 1 + fromEnum (shape result) + case state result of Loss -> 0
                                                                     Draw -> 3
                                                                     Win  -> 6
