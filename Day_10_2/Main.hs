{-# LANGUAGE PatternGuards #-}

module Main where

import Data.List (foldl', foldl1', sort, subsequences)
import Data.Maybe (fromMaybe)

(|>) :: a -> (a -> c) -> c
(|>) = flip ($)


differences :: [Integer] -> [Integer]
differences = snd . foldl' f (0, [])
    where
        f :: (Integer, [Integer]) -> Integer -> (Integer, [Integer])
        f (left, accu) right = (right, (right - left):accu)


runLengthEncode :: (Eq a) => [a] -> [(a, Integer)]
runLengthEncode [] = []
runLengthEncode (a:as) = reverse $ (lastA, lastN) : rs
    where
        (lastA, lastN, rs) = foldl' f (a, 1, []) as
        f :: (Eq a) => (a, Integer, [(a, Integer)]) -> a -> (a, Integer, [(a, Integer)])
        f (a, n, lst) b
            | a == b = (a, n+1, lst)
            | otherwise = (b, 1, (a,n):lst)

possibleSubs as = sort as
    |> subsequences
    |> filter (\x -> 0 `elem` x && maximum as `elem` x)
    |> filter (not.null.differences)
    |> filter ((==3).maximum.differences)

tribonacci :: Integer -> Integer
tribonacci 0 = 0
tribonacci 1 = 0
tribonacci 2 = 1
tribonacci n = tribonacci (n-1) + tribonacci (n-2) + tribonacci (n-3)

{-
We can drop an adapter (and thus a difference) each time there's a series of ones.
So we take each consecutive sequence of ones. Denote the length of such a sequence by n.
We need to count the number of consecutive subsequences of length <= 2 - these are the ones we can drop.
The number of these is tri(n+2), where tri denotes the sequence of tribonacci numbers.
For each possible subsequences the "tree" of possible adapter combinations fans out -
and thus the number of current combinations multiplies by the possible choices.
-}
countPossibleChains :: [Integer] -> Integer
countPossibleChains ds = foldl' f 1 rle
    where
        rle = runLengthEncode ds
        f :: Integer -> (Integer, Integer) -> Integer
        f n (difference, count)
            | difference == 1 = n * numberOfPossibleSubsequences count
            | otherwise = n
        numberOfPossibleSubsequences :: Integer -> Integer
        numberOfPossibleSubsequences k = tribonacci (k+2)

main :: IO ()
main = do
    text <- readFile "input.txt"
    let voltages = text |> lines |> map read :: [Integer]
    let builtInAdapter = 3 + maximum voltages
    let chargingPoint = 0
    (chargingPoint:builtInAdapter:voltages) 
        |> sort
        |> differences
        |> countPossibleChains
        |> print