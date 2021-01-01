module Main where

import qualified Data.Set as Set
import Data.List (foldl')
import Data.Maybe (catMaybes)

(|>) :: a -> (a -> c) -> c
(|>) = flip ($)

type Row = Integer
type Column = Integer
data Seat = Seat Row Column deriving (Show)

lastSeat = Seat 127 7
highestId = seatId lastSeat

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

findSeats :: Set.Set Integer -> Integer
findSeats idsInPlane = case Set.toList candidates of
        [x] -> x
        _ -> error "Found multiple valid seats"
    where
        candidates = foldl' f Set.empty [0..highestId]
        f possibleIds seatId
            | possibleSeat seatId = Set.insert seatId possibleIds
            | otherwise = possibleIds
        possibleSeat :: Integer -> Bool
        possibleSeat seat = necessaryIds `Set.isSubsetOf` idsInPlane
            && seat `Set.notMember` idsInPlane
            where
                necessaryIds = Set.fromAscList [seat-1, seat+1]

main :: IO ()
main = do
    text <- readFile "input.txt"
    text
        |> lines
        |> map (seatId.boardingPassToSeat)
        |> Set.fromList
        |> findSeats
        |> print
        