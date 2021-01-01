module Main where

-- Cartesian power for homogenous data
homoCartPow :: [a] -> Integer -> [[a]]
homoCartPow xs 1 = map (:[]) xs
homoCartPow xs n = [x:ys | x <- xs, ys <- homoCartPow xs (n-1)]

(|>) = flip ($)

main :: IO ()
main = do
    text <- readFile "input.txt"
    text |>
        lines |>
        map (read :: String -> Integer) |>
        (`homoCartPow` 3) |>
        filter (\xs -> sum xs == 2020) |>
        (!!1) |>
        product |>
        print
