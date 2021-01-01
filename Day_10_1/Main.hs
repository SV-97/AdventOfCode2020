module Main where

import Data.List (foldl', sort)

(|>) :: a -> (a -> c) -> c
(|>) = flip ($)


differences :: [Integer] -> [Integer]
differences = snd . foldl' f (0, [])
    where
        f :: (Integer, [Integer]) -> Integer -> (Integer, [Integer])
        f (left, accu) right = (right, (right - left):accu)


main :: IO ()
main = do
    text <- readFile "input.txt"
    let voltages = text |> lines |> map read :: [Integer]
    let builtInAdapter = 3 + maximum voltages
    let chargingPoint = 0
    (chargingPoint:builtInAdapter:voltages) 
        |> sort
        |> differences
        |> (\ds ->let
            threes = length $ filter (==3) ds
            ones = length $ filter (==1) ds
            in ones * threes
            )
        |> print