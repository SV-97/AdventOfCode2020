module Main where

import qualified Data.Set as Set
import qualified Data.Foldable as Foldable

(|>) :: a -> (a -> c) -> c
(|>) = flip ($)

splitAtChar :: Char -> String -> (String, String)
splitAtChar c s = (a, tail b)
    where (a, b) = span (/=c) s

trySplitAtChar :: Char -> String -> Maybe (String, String)
trySplitAtChar c s
    | null b = Nothing
    | otherwise = Just (a, tail b)
    where (a, b) = span (/=c) s

trySplitAtSubString :: String -> String -> Maybe (String, String)
trySplitAtSubString [] _ = undefined
trySplitAtSubString [c1] s = trySplitAtChar c1 s
trySplitAtSubString css@(c:cs) s = do
        (a, b) <- trySplitAtChar c s
        let len = length cs
        if take len b == cs
        then Just (a, drop len b)
        else do
                (a', b') <- trySplitAtSubString css b
                pure (a ++ c : a', b')

-- similar to lines but splits on arbitrary substrings
substrings :: String -> String -> [String]
substrings sub text = case trySplitAtSubString sub text of
    Nothing -> [text]
    (Just (a, b)) -> a : substrings sub b

count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)

countPred :: (a -> Bool) -> [a] -> Int
countPred pred = length . filter pred

intersections = Foldable.foldl1 Set.intersection 

answersToGroup :: [String] -> Set.Set Char
answersToGroup = intersections . map Set.fromList

countGroups :: [[String]] -> Int
countGroups groups = sum [Set.size $ answersToGroup group | group <- groups]

main :: IO ()
main = do
    text <- readFile "input.txt"
    text
        |> substrings "\n\n" -- two newlines signify a new entry
        |> map lines
        |> countGroups
        |> print
        