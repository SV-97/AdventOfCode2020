{-# LANGUAGE PatternGuards #-}

module Main where

import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.List (groupBy)

(|>) :: a -> (a -> c) -> c
(|>) = flip ($)

cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct xs ys = [(x,y) | x <- xs, y <- ys]

cartesianPow :: [b] -> [(b, b)]
cartesianPow xs = cartesianProduct xs xs

countBy :: (a -> Bool) -> [a] -> Int
countBy pred = length . filter pred

enumerate :: [b] -> [(Integer, b)]
enumerate = zip [0..]

data Seat = Floor | Empty | Occupied  deriving (Eq)

occupied :: Seat -> Bool
occupied Occupied = True
occupied _ = False

instance Show Seat where
    show Floor = "."
    show Occupied = "#"
    show Empty = "L"

data Grid = RowGrid [(Integer, Row)] | MapGrid (Map.Map (Integer,Integer) Seat) deriving (Eq)
instance Show Grid where
    show (RowGrid r) = unlines $ map (show.snd) r
    show g@(MapGrid _) = show $ toRowGrid g

newtype Row = Row [(Integer, Seat)]  deriving (Eq)
instance Show Row where
    show (Row r) = concatMap (show.snd) r

toMapGrid :: Grid -> Grid
toMapGrid (RowGrid rs) = MapGrid $ Map.fromList $ concat [[((i,j), seat) | (j, seat) <- seats] | (i, Row seats) <- rs]
toMapGrid x = x

toRowGrid :: Grid -> Grid
toRowGrid (MapGrid m) = RowGrid [(i, Row [(j, seat) | (j, seat) <- enumerate group]) | (i, group) <- enumerate gridElems]
    where gridElems = map (map snd) $ groupBy (\x y -> fst (fst x) == fst (fst y)) $ Map.toAscList m
toRowGrid x = x

parseGrid :: [String] -> Grid
parseGrid ls = toMapGrid $ RowGrid [(i, parseRow row) | (i, row) <- enumerate ls]

parseRow :: String -> Row
parseRow cs = Row [(i, parseSeat c) | (i, c) <- enumerate cs]

parseSeat :: Char -> Seat
parseSeat '.' = Floor
parseSeat '#' = Occupied
parseSeat 'L' = Empty
parseSeat x = error $ "Invalid Character: " ++ show x

adjacentSeats :: (Integer, Integer) -> Grid -> [((Integer, Integer), Seat)]
adjacentSeats (i,j) g@(RowGrid _) = adjacentSeats (i,j) (toMapGrid g)
adjacentSeats (i,j) (MapGrid m) = mapMaybe f validCoords
    where
        f coords = do
            seat <- coords `Map.lookup` m
            pure (coords, seat) 
        validCoords = mapMaybe (\(x,y) -> if x /= 0 || y /= 0 then Just (i+x, j+y) else Nothing) $ cartesianPow [-1, 0, 1]

transformGrid :: (Seat -> Seat) -> Grid -> Grid
transformGrid f g@(RowGrid _) = transformGrid f (toMapGrid g)
transformGrid f (MapGrid m) = MapGrid (Map.map f m)

transformGridWithCoords :: ((Integer, Integer) -> Seat -> Seat) -> Grid -> Grid
transformGridWithCoords f g@(RowGrid _) = transformGridWithCoords f (toMapGrid g)
transformGridWithCoords f (MapGrid m) = MapGrid (Map.mapWithKey f m)

applyRules :: Grid -> Grid
applyRules grid = transformGridWithCoords g grid
    where
        g :: (Integer, Integer) -> Seat -> Seat
        g idx Empty | all (not.occupied) adj = Occupied
            where adj = map snd $ adjacentSeats idx grid
        g idx Occupied | countBy occupied adj >= 4 = Empty
            where adj = map snd $ adjacentSeats idx grid
        g _ x = x

fix :: (Eq a) => (a -> a) -> a -> a
fix f a
    | f a == a = a
    | otherwise = fix f (f a)

countOccupied :: Grid -> Int
countOccupied (MapGrid m) = Map.elems m |> countBy occupied
countOccupied g = countOccupied (toMapGrid g)

main :: IO ()
main = do
    text <- readFile "input.txt"
    text
        |> lines
        |> parseGrid
        |> fix applyRules
        |> countOccupied
        |> print



