#!/usr/bin/env stack
{- stack script
 --resolver lts-20.3
 --install-ghc
 --package "containers"
-}

import Data.Function
import qualified Data.List as List
import Data.Maybe
import qualified Data.Set as Set
import System.Environment

slidingWindow :: Int -> [a] -> [[a]]
slidingWindow _ [] = []
slidingWindow size list@(_:others)
    | length currentSlice == size = currentSlice : (slidingWindow size others)
    | otherwise = []
    where currentSlice = take size list

main = do
    (inputPath:_) <- getArgs
    contents <- readFile inputPath
    let windowSize = 4
    print (contents & 
        init & 
        slidingWindow windowSize & 
        List.findIndex areElementsUnique & 
        fromJust &
        (+ windowSize))

areElementsUnique :: Ord a => [a] -> Bool
areElementsUnique list = length (Set.fromList list) == length list
