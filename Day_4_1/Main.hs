module Main where

-- Cartesian power for homogenous data
homoCartPow :: [a] -> Integer -> [[a]]
homoCartPow xs 1 = map (:[]) xs
homoCartPow xs n = [x:ys | x <- xs, ys <- homoCartPow xs (n-1)]

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

countTrees :: (Int, Int) -> (Int, Int) -> [String] -> Int
countTrees _ _ [] = 0
countTrees (xSlope, ySlope) (x, y) ls
    | length ls <= y + ySlope = 0
    | line !! (x + xSlope) == '#' = 1 + f'
    | otherwise = f'
        where
            line = ls !! (y + ySlope)
            f' = countTrees (xSlope, ySlope) (x + xSlope, y + ySlope) ls

toEntry :: String -> [(String, String)]
toEntry s = [splitAtChar ':' w | w <- words s]

validPassport :: [(String, String)] -> Bool
validPassport pp = and [key `elem` ppKeys | key <- ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"{-, "cid" is optional-}]]
    where ppKeys = map fst pp

main :: IO ()
main = do
    text <- readFile "input.txt"
    text
        |> substrings "\n\n" -- two newlines signify a new entry
        |> map toEntry
        |> countPred validPassport
        |> print
