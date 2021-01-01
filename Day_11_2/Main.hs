{-# LANGUAGE PatternGuards #-}

module Main where

import qualified Data.Map as Map
import Data.Map ((!))
import Data.Maybe (mapMaybe, isJust)
import Data.List (groupBy, delete)
import Prelude hiding (floor, pred)

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

data Seat = Floor | Empty | Occupied  deriving (Eq)

occupied :: Seat -> Bool
occupied Occupied = True
occupied _ = False

floor :: Seat -> Bool
floor Floor = True
floor _ = False

instance Show Seat where
    show Floor = "."
    show Occupied = "#"
    show Empty = "L"

type Index = (Int, Int)
type Valids = [(Index, Seat)]

data Grid
    = RowGrid [(Int, Row)]
    | MapGrid (Map.Map Index Seat) deriving (Eq)

instance Show Grid where
    show (RowGrid r) = unlines $ map (show.snd) r
    show g@(MapGrid _) = show $ toRowGrid g

newtype Row = Row [(Int, Seat)]  deriving (Eq)
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

inGrid :: Index -> Grid -> Bool
inGrid coords g@(RowGrid _) = inGrid coords (toMapGrid g)
inGrid (y,x) (MapGrid m) = minY <= y && y <= maxY && minX <= x && x <= maxX
    where
        k = Map.keys m
        (minY, minX) = minimum k
        (maxY, maxX) = maximum k

tryHead :: [a] -> Maybe a
tryHead (a:_) = Just a
tryHead [] = Nothing

validSeats :: Index -> Grid -> Valids
validSeats (i,j) g@(RowGrid _) = validSeats (i,j) (toMapGrid g)
validSeats (i,j) g@(MapGrid m) = mapMaybe f validCoords
    where
        f coords = do
            seat <- coords `Map.lookup` m
            pure (coords, seat) 
        -- we cast rays in all directions eminating from the index
        directions = map (\(x,y) -> iterate (\(i',j') -> (i' + x, j' + y)) (i,j)) $ delete (0,0) $ cartesianPow [-1, 0, 1]
        -- and find their first "hit"
        validCoords = mapMaybe (tryHead . dropWhile seatAtCoordFloor . takeWhile (`inGrid` g) . tail) directions
        seatAtCoordFloor :: Index -> Bool
        seatAtCoordFloor coords = isJust $ do
            seat <- coords `Map.lookup` m
            if floor seat
            then Just ()
            else Nothing

validIndices :: Index -> Grid -> [Index]
validIndices idx g = map fst $ validSeats idx g

transformGrid :: (Seat -> Seat) -> Grid -> Grid
transformGrid f g@(RowGrid _) = transformGrid f (toMapGrid g)
transformGrid f (MapGrid m) = MapGrid (Map.map f m)

transformGridWithCoords :: (Index -> Seat -> Seat) -> Grid -> Grid
transformGridWithCoords f g@(RowGrid _) = transformGridWithCoords f (toMapGrid g)
transformGridWithCoords f (MapGrid m) = MapGrid (Map.mapWithKey f m)

applyRules :: Grid -> Grid
applyRules grid = transformGridWithCoords g grid
    where
        g :: Index -> Seat -> Seat
        g idx Empty | all (not.occupied) vld = Occupied
            where vld = map snd $ validSeats idx grid
        g idx Occupied | countBy occupied vld >= 5 = Empty
            where vld = map snd $ validSeats idx grid
        g _ x = x


{-
We notice that the floor spots are invariant under the rules.
So instead of calculating them new each iteration we instead
compute them once and look them up.
-}
applyRulesWithValids :: Grid -> Map.Map Index [Index] -> Grid
applyRulesWithValids grid@(RowGrid _) valids = applyRulesWithValids (toMapGrid grid) valids
applyRulesWithValids grid@(MapGrid m) valids = transformGridWithCoords g grid
    where
        g :: Index -> Seat -> Seat
        g idx Empty | all (not.occupied) vld = Occupied
            where vld = map (m !) $ valids ! idx
        g idx Occupied | countBy occupied vld >= 5 = Empty
            where vld = map (m !) $ valids ! idx
        g _ x = x

fix :: (Eq a) => (a -> a) -> a -> a
fix f a
    | f a == a = a
    | otherwise = fix f (f a)

fixWith :: (Eq a) => (a -> b -> a) -> a -> b -> a
fixWith f a b
    | f a b == a = a
    | otherwise = fixWith f (f a b) b

countOccupied :: Grid -> Int
countOccupied (MapGrid m) = Map.elems m |> countBy occupied
countOccupied g = countOccupied (toMapGrid g)

app :: (Eq t1, Num t1) => (t2 -> t2) -> t2 -> t1 -> t2
app f x 0 = x
app f x k = app f (f x) (k-1)

validsMap :: Grid -> Map.Map Index [Index]
validsMap g@(MapGrid m) = Map.fromList $ map (tee (`validIndices` g)) (Map.keys m)
validsMap g@(RowGrid _) = validsMap $ toMapGrid g

tee :: (a -> b) -> a -> (a, b)
tee f a = (a, f a)

main :: IO ()
main = do
    text <- readFile "input.txt"
    text
        |> lines
        |> parseGrid
        |> tee validsMap
        |> uncurry (fixWith applyRulesWithValids)
        |> countOccupied
        |> print
