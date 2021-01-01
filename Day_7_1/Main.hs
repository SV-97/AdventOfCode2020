module Main where

import qualified Data.Set as Set
import Data.List
import Data.Maybe (isJust)

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

type Adjective = String
type Color = String
data Bag = Bag Adjective Color deriving (Eq, Show)

shinyGold :: Bag
shinyGold = Bag "shiny" "gold"
data Rule = Rule Bag [(Bag, Integer)] deriving Show

outerBag (Rule o _) = o

parseRule :: String -> Rule
parseRule raw = Rule outerBag rules
    where
        (oAdj:oCol:_:_:innerBags) = words (filter (/=',') raw)
        outerBag = Bag oAdj oCol
        rules = unfoldr f innerBags
        f [] = Nothing
        f ("no":_) = Nothing
        f (n:adj:color:_:rest) = Just ((Bag adj color, read n), rest)
        f _ = error ""

uniqueList [] = []
uniqueList (x:xs) = x : uniqueList (filter (/=x) xs)

countValidBags :: [Rule] -> Integer
countValidBags rules = fromIntegral $ length $ uniqueList lst
    where lst = filter (canEventuallyContainShinyGold rules) (map outerBag rules)

canEventuallyContainShinyGold :: [Rule] -> Bag -> Bool
canEventuallyContainShinyGold rules bag = isJust (lookup shinyGold rulesForBag) 
    || or [canEventuallyContainShinyGold rules bag | (bag, _) <- rulesForBag]
    where
        rulesForBag = concatMap (\(Rule _ rs) -> rs) $ filter ((==bag).outerBag) rules

main :: IO ()
main = do
    text <- readFile "input.txt"
    text
        |> lines
        |> map init -- get rid of dot
        |> map parseRule
        |> countValidBags
        |> print
        