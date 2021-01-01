{-# LANGUAGE PatternGuards #-}

module Main where

import Control.Monad (foldM)
import Data.Either (fromLeft)
import Data.List (subsequences, foldl')

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

-- map (head $ map snd $ dropWhile ((/=invalidNumber).fst) $ scanl (\(sum, accu) n -> (n+sum, n:accu)) (0, [])) $ [drop k ns | k <- [0..length ns]]

crackXmas :: [Integer] -> Integer
crackXmas ns = max + min
    where
        max = maximum range
        min = minimum range
        range = init [drop k ns | k <- [0..length ns]] -- consider all starting positions
            |> map (scanl (\(sum, accu) n -> (n+sum, n:accu)) (0, [])) -- find cumulative sums for each
            |> map (dropWhile ((/=invalidNumber).fst)) -- drop sequences that don't sum to the invalidNumber
            |> dropWhile null -- drop starting positions that have no valid sequences
            |> map (head . map snd) -- take the first possible solution
            |> head
        invalidNumber = fromLeft undefined $ foldM process (reverse preamble) rest
        (preamble, rest) = splitAt preambleLength ns
        
main :: IO ()
main = do
    text <- readFile "input.txt"
    text
        |> lines
        |> map read
        |> crackXmas
        |> print
