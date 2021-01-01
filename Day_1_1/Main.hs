module Main where

cartesianProduct xs ys = [(x,y) | x <- xs, y <- ys]

(|>) = flip ($)

main :: IO ()
main = do
    text <- readFile "input.txt"
    text |>
        lines |>
        map (read :: String -> Integer) |>
        \x -> cartesianProduct x x |>
        filter ((==2020) . uncurry (+)) |>
        (!!1) |>
        uncurry (*) |>
        print
