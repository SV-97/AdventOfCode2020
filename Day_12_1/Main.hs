{-# LANGUAGE PatternGuards #-}

module Main where

import Data.List (foldl')

(|>) :: a -> (a -> c) -> c
(|>) = flip ($)

cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct xs ys = [(x,y) | x <- xs, y <- ys]

cartesianPow :: [b] -> [(b, b)]
cartesianPow xs = cartesianProduct xs xs

countBy :: (a -> Bool) -> [a] -> Int
countBy pred = length . filter pred

enumerate :: [b] -> [(Int, b)]
enumerate = zip [0..]

data Action
    = MoveNorth Int
    | MoveSouth Int
    | MoveEast Int
    | MoveWest Int
    | MoveForward Int
    | TurnLeft Int
    | TurnRight Int

parseAction :: String -> Action
parseAction ('N':n) = MoveNorth $ read n
parseAction ('S':n) = MoveSouth $ read n
parseAction ('E':n) = MoveEast $ read n
parseAction ('W':n) = MoveWest $ read n
parseAction ('F':n) = MoveForward $ read n
parseAction ('L':n) = TurnLeft $ read n `div` 90
parseAction ('R':n) = TurnRight $ read n `div` 90
parseAction _ = undefined

data Direction
    = North
    | South
    | East
    | West
    deriving Show

data Point = Cartesian {x :: Int, y :: Int} deriving Show
data Ship = Ship {direction :: Direction, location :: Point} deriving Show

rotate :: Direction -> Action -> Direction
rotate x (TurnLeft 0) = x
rotate x (TurnRight 0) = x
rotate North (TurnLeft n) = rotate West (TurnLeft (n - 1))
rotate North (TurnRight n) = rotate East (TurnRight (n - 1))
rotate South (TurnLeft n) = rotate East (TurnLeft (n - 1))
rotate South (TurnRight n) = rotate West (TurnRight (n - 1))
rotate East (TurnLeft n) = rotate North (TurnLeft (n - 1))
rotate East (TurnRight n) = rotate South (TurnRight (n - 1))
rotate West (TurnLeft n) = rotate South (TurnLeft (n - 1))
rotate West (TurnRight n) = rotate North (TurnLeft (n - 1))
rotate _ _ = undefined

applyAction :: Ship -> Action -> Ship
applyAction ship (MoveNorth n) = ship {location = (location ship) {y = y (location ship) + n}}
applyAction ship (MoveSouth n) = ship {location = (location ship) {y = y (location ship) - n}}
applyAction ship (MoveEast n) = ship {location =  (location ship) {x = x (location ship) + n}}
applyAction ship (MoveWest n) = ship {location =  (location ship) {x = x (location ship) - n}}
applyAction ship (MoveForward n) = case direction ship of
    North -> applyAction ship (MoveNorth n)
    South -> applyAction ship (MoveSouth n)
    East -> applyAction ship (MoveEast n)
    West -> applyAction ship (MoveWest n)
applyAction ship t = ship {direction = rotate (direction ship) t} -- turns

manhattanDistance :: Point -> Point -> Int
manhattanDistance (Cartesian x1 y1) (Cartesian x2 y2) = abs (x1 - x2) + abs (y1 - y2)

origin :: Point
origin = Cartesian 0 0

main :: IO ()
main = do
    text <- readFile "input.txt"
    text
        |> lines
        |> map parseAction
        |> foldr (flip applyAction) (Ship East origin)
        |> (\x -> (x,manhattanDistance origin (location x)))
        |> print
