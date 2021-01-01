module Main where

import Data.Char (toUpper, isDigit)
import Data.Maybe (catMaybes)

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

data Height = Cm Int | In Int deriving (Show)
type Year = Int
type HairColor = String
data EyeColor = Amb | Blu | Brn | Gry | Grn | Hzl | Oth deriving (Show, Read)
data Passport = Valid Year Year Year Height String EyeColor String deriving Show
validatedPassport byr iyr eyr hgt hcl ecl pid = do
    assert $ 1920 <= byr && byr <= 2002
    assert $ 2010 <= iyr && iyr <= 2020
    assert $ 2020 <= eyr && eyr <= 2030
    assert (case hgt of
        (Cm x) -> 150 <= x && x <= 193
        (In x) -> 59 <= x && x <= 76)
    assert (length hcl == 7
        && head hcl == '#'
        && let t = tail hcl
            in all (`elem` ['0'..'9'] ++ ['a'..'f']) t)
    assert (length pid == 9
        && all isDigit pid)
    Just $ Valid byr iyr eyr hgt hcl ecl pid


tryIndex :: [a] -> Int -> Maybe a
tryIndex [] _ = Nothing
tryIndex (x:_) 0 = Just x
tryIndex (_:xs) n = tryIndex xs (n-1)

tryRead :: Read a => String -> Maybe a
tryRead = fmap fst . tryReads

tryReads :: Read a => String -> Maybe (a, String)
tryReads = (`tryIndex` 0) . reads

assert :: Bool -> Maybe ()
assert True = Just ()
assert False = Nothing

tryParsePassport :: [(String, String)] -> Maybe Passport
tryParsePassport as = do
    byr <- lookup "byr" as >>= tryRead
    iyr <- lookup "iyr" as >>= tryRead
    eyr <- lookup "eyr" as >>= tryRead
    hgt'' <- lookup "hgt" as
    hcl <- lookup "hcl" as
    ecl' <- lookup "ecl" as
    pid <- lookup "pid" as
    (hgt', unit) <- tryReads hgt''
    hgt <- case unit of
        "cm" -> Just (Cm hgt')
        "in" -> Just (In hgt')
        _ -> Nothing
    ecl <- tryRead (capitalize ecl')
    validatedPassport byr iyr eyr hgt hcl ecl pid
        where
            capitalize [] = []
            capitalize (x:xs) = toUpper x : xs

main :: IO ()
main = do
    text <- readFile "input.txt"
    text
        |> substrings "\n\n" -- two newlines signify a new entry
        |> map toEntry
        |> map tryParsePassport
        |> catMaybes
        |> length
        |> print
