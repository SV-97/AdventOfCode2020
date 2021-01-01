module Main where

import qualified Data.Map as Map
import Data.Maybe (mapMaybe, isJust)
import Data.List (groupBy, delete)
import Prelude hiding (floor, pred)
import qualified Data.Array as Array
import Data.Array ((!))

(|>) :: a -> (a -> c) -> c
(|>) = flip ($)

cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct xs ys = [(x,y) | x <- xs, y <- ys]

cartesianPow :: [b] -> [(b, b)]
cartesianPow xs = cartesianProduct xs xs

countBy :: (a -> Bool) -> [a] -> Int
countBy pred = length . filter pred

enumerate :: Integral a => [b] -> [(a, b)]
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

data Grid
    = RowGrid [(Int, Row)]
    | MapGrid (Map.Map Index Seat)
    | ArrayGrid (Array.Array Index Seat) deriving (Eq)
instance Show Grid where
    show (RowGrid r) = unlines $ map (show.snd) r
    show g = show $ toRowGrid g

newtype Row = Row [(Int, Seat)]  deriving (Eq)
instance Show Row where
    show (Row r) = concatMap (show.snd) r

toMapGrid :: Grid -> Grid
toMapGrid (RowGrid rs) = MapGrid $ Map.fromList $ concat [[((i,j), seat) | (j, seat) <- seats] | (i, Row seats) <- rs]
toMapGrid (ArrayGrid a) = MapGrid $ Map.fromList $ map (\((x,y), e) -> ((fromIntegral x, fromIntegral y), e)) $ Array.assocs a
toMapGrid g@(MapGrid _) = g

toRowGrid :: Grid -> Grid
toRowGrid (MapGrid m) = RowGrid [(i, Row [(j, seat) | (j, seat) <- enumerate group]) | (i, group) <- enumerate gridElems]
    where gridElems = map (map snd) $ groupBy (\x y -> fst (fst x) == fst (fst y)) $ Map.toAscList m
toRowGrid g@(ArrayGrid _) = toRowGrid $ toMapGrid g
toRowGrid g@(RowGrid _) = g

toArrayGrid :: Grid -> Grid
toArrayGrid g@(ArrayGrid _) = g
toArrayGrid g@(RowGrid _) = toArrayGrid $ toMapGrid g
toArrayGrid (MapGrid m) = ArrayGrid $ Array.array ((0, yMax), (0, xMax)) vals
    where
        vals = map (\((x,y), e) -> ((fromIntegral x, fromIntegral y), e)) $ Map.toList m
        xMax = fromIntegral $ maximum $ map snd $ Map.keys m
        yMax = fromIntegral $ maximum $ map fst $ Map.keys m

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
inGrid coords g@(RowGrid _) = inGrid coords (toArrayGrid g)
inGrid (y,x) (ArrayGrid a) 
    = minY <= y
    && y <= maxY
    && minX <= x
    && x <= maxX
    where
        ((minY, minX), (maxY, maxX)) = Array.bounds a
inGrid (y,x) (MapGrid m)
    = minY <= y
    && y <= maxY
    && minX <= x
    && x <= maxX
    where
        k = Map.keys m
        (minY, minX) = minimum k
        (maxY, maxX) = maximum k


tryHead :: [a] -> Maybe a
tryHead (a:_) = Just a
tryHead [] = Nothing

validSeats :: Index -> Grid -> [(Index, Seat)]
validSeats (i,j) g@(RowGrid _) = validSeats (i,j) (toMapGrid g)
validSeats (i,j) g@(ArrayGrid a) = map f validCoords
    where
        f :: (Int, Int) -> ((Int, Int), Seat)
        f coords = (coords, a ! coords) 
        directions = map (\(x,y) -> iterate (\(i',j') -> (i' + x, j' + y)) (i,j)) $ delete (0,0) $ cartesianPow [-1, 0, 1]
        validCoords = mapMaybe (tryHead . dropWhile seatAtCoordFloor . takeWhile (`inGrid` g) . tail) directions
        seatAtCoordFloor :: (Int, Int) -> Bool
        seatAtCoordFloor coords = coords `inGrid` g && floor (a ! coords)
validSeats (i,j) g@(MapGrid m) = mapMaybe f validCoords
    where
        f coords = do
            seat <- coords `Map.lookup` m
            pure (coords, seat) 
        directions = map (\(x,y) -> iterate (\(i',j') -> (i' + x, j' + y)) (i,j)) $ delete (0,0) $ cartesianPow [-1, 0, 1]
        validCoords = mapMaybe (tryHead . dropWhile seatAtCoordFloor . takeWhile (`inGrid` g) . tail) directions
        seatAtCoordFloor :: Index -> Bool
        seatAtCoordFloor coords = isJust $ do
            seat <- coords `Map.lookup` m
            if floor seat
            then Just ()
            else Nothing


transformGrid :: (Seat -> Seat) -> Grid -> Grid
transformGrid f g@(RowGrid _) = transformGrid f (toMapGrid g)
transformGrid f (MapGrid m) = MapGrid (Map.map f m)
transformGrid f (ArrayGrid a) = ArrayGrid (fmap f a)

transformGridWithCoords :: (Index -> Seat -> Seat) -> Grid -> Grid
transformGridWithCoords f g@(RowGrid _) = transformGridWithCoords f (toMapGrid g)
transformGridWithCoords f (MapGrid m) = MapGrid (Map.mapWithKey f m)
transformGridWithCoords f (ArrayGrid a) = ArrayGrid $ Array.array bounds (map g lst)
    where
        bounds = Array.bounds a
        lst = Array.assocs a
        g (i, s) = (i, f i s)

applyRules :: Grid -> Grid
applyRules grid = transformGridWithCoords g grid
    where
        g :: Index -> Seat -> Seat
        g idx Empty | all (not.occupied) vld = Occupied
            where vld = map snd $ validSeats idx grid
        g idx Occupied | countBy occupied vld >= 5 = Empty
            where vld = map snd $ validSeats idx grid
        g _ x = x

fix :: (Eq a) => (a -> a) -> a -> a
fix f a
    | f a == a = a
    | otherwise = fix f (f a)

countOccupied :: Grid -> Int
countOccupied (ArrayGrid a) = Array.elems a |> countBy occupied
countOccupied (MapGrid m) = Map.elems m |> countBy occupied
countOccupied g = countOccupied (toMapGrid g)

app f x 0 = x
app f x k = app f (f x) (k-1)

main :: IO ()
main = do
    text <- readFile "input.txt"
    text
        |> lines
        |> parseGrid
        |> fix applyRules
        |> countOccupied
        |> print

test = ".......#.\n...#.....\n.#.......\n.........\n..#L....#\n....#....\n.........\n#........\n...#....."
test2 = ".............\n.L.L.#.#.#.#.\n............."

test3 = "L.LL.LL.LL\nLLLLLLL.LL\nL.L.L..L..\nLLLL.LL.LL\nL.LL.LL.LL\nL.LLLLL.LL\n..L.L.....\nLLLLLLLLLL\nL.LLLLLL.L\nL.LLLLL.LL" |> lines |> parseGrid