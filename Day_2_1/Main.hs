module Main where

-- Cartesian power for homogenous data
homoCartPow :: [a] -> Integer -> [[a]]
homoCartPow xs 1 = map (:[]) xs
homoCartPow xs n = [x:ys | x <- xs, ys <- homoCartPow xs (n-1)]

(|>) = flip ($)

splitAtChar :: Char -> String -> (String, String)
splitAtChar c s = (a, tail b)
    where (a, b) = span (/=c) s

data PasswordEntry = Entry Int Int Char String deriving Show

instance Read PasswordEntry where
    readsPrec _ s = [(Entry (read a) (read b) letter pw, "")]
        where
            [numbers, letter:_, pw] = words s
            (a, b) = splitAtChar '-' numbers

count x = length . filter (==x)

validEntry :: PasswordEntry -> Bool
validEntry (Entry l u c s) = l <= n && n <= u
    where n = count c s

main :: IO ()
main = do
    text <- readFile "input.txt"
    text |>
        lines |>
        map read |>
        filter validEntry |>
        length |>
        print
