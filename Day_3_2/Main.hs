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

countTrees :: (Int, Int) -> (Int, Int) -> [String] -> Int
countTrees _ _ [] = 0
countTrees (xSlope, ySlope) (x, y) ls
    | length ls <= y + ySlope = 0
    | line !! (x + xSlope) == '#' = 1 + f'
    | otherwise = f'
        where
            line = ls !! (y + ySlope)
            f' = countTrees (xSlope, ySlope) (x + xSlope, y + ySlope) ls

main :: IO ()
main = do
    text <- readFile "input.txt"
    text
        |> lines
        |> map cycle
        |> (\grid -> product [countTrees slope (0,0) grid | slope <- [(1,1), (3,1), (5,1), (7,1), (1,2)]])
        |> print
