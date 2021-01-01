module Main where

-- Cartesian power for homogenous data
homoCartPow :: [a] -> Integer -> [[a]]
homoCartPow xs 1 = map (:[]) xs
homoCartPow xs n = [x:ys | x <- xs, ys <- homoCartPow xs (n-1)]

(|>) = flip ($)

splitAtChar :: Char -> String -> (String, String)
splitAtChar c s = (a, tail b)
    where (a, b) = span (/=c) s

count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)

countTrees :: (Int, Int) -> [String] -> Int
countTrees _ [] = 0
countTrees _ [_] = 0
countTrees (x, y) (_:line:ls)
    | (line !! (x + 3)) == '#' = 1 + countTrees (x+3, y+1) (line:ls)
    | otherwise = countTrees (x+3, y+1) (line:ls)

main :: IO ()
main = do
    text <- readFile "input.txt"
    text
        |> lines
        |> map cycle
        |> countTrees (0,0)
        |> print
