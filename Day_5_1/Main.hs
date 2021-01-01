module Main where

(|>) :: a -> (a -> c) -> c
(|>) = flip ($)

type Row = Integer
type Column = Integer
data Seat = Seat Row Column deriving (Show)

seatId :: Seat -> Integer
seatId (Seat r c) = 8 * r + c

boardingPassToSeat :: String -> Seat
boardingPassToSeat s = Seat row col
    where
        (row', col') = splitAt 7 s
        row = binaryFrom (=='B') row'
        col = binaryFrom (=='R') col'
        binaryFrom :: Eq a => (a -> Bool) -> [a] -> Integer
        binaryFrom p lst = sum [if p a then 2^k else 0 | (k, a) <- zip [0..] (reverse lst)]

main :: IO ()
main = do
    text <- readFile "input.txt"
    text
        |> lines
        |> map (seatId.boardingPassToSeat)
        |> maximum
        |> print
