module Main where

import Control.Monad (foldM)
import Data.Either (fromLeft)

(|>) :: a -> (a -> c) -> c
(|>) = flip ($)

cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct xs ys = [(x,y) | x <- xs, y <- ys]

enumerate :: [b] -> [(Integer, b)]
enumerate = zip [0..]

process :: [Integer] -> Integer -> Either Integer [Integer]
process previous n
    | n `elem` sums = Right (n : init previous)
    | otherwise = Left n
    where
        ps = enumerate previous
        validPairs = [(k,l) |((i,k), (j, l)) <- cartesianProduct ps ps, i /= j]
        sums = map (uncurry (+)) validPairs

preambleLength :: Int
preambleLength = 25

crackXmas :: [Integer] -> Integer
crackXmas ns = fromLeft undefined $ foldM process (reverse preamble) rest
    where
        (preamble, rest) = splitAt preambleLength ns

main :: IO ()
main = do
    text <- readFile "input.txt"
    text
        |> lines
        |> map read
        |> crackXmas
        |> print
