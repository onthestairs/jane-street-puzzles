module Main where

import Data.Matrix
import Data.List (nub, sortBy)
import Data.Maybe (catMaybes, fromJust)
import Data.Function (on)
import qualified Debug.Trace as Trace

data Op = Add | Times | Divide | Subtract
type Target = Int
type Offset = Int
type Length = Int
type Row = (Offset, Length)
data KenKenRange = KenKenRange Op Target [Row]
type Coords = (Int, Int)
type KenKenPiece = (Coords, KenKenRange)
data KenKen = KenKen [KenKenPiece]
type SolvedKenKen = Matrix Int
type PartialKenKen = Matrix (Maybe Int)

remove element = filter (/= element)
combinations _ 0 = [[]]
combinations xs n = [x:cs | x <- xs, cs <- combinations (remove x xs) (n-1)]
rowsCombination = combinations [1..9]
rowsCombinations [] = [[]]
rowsCombinations (n:ns) = [c:next | c <- rowsCombination n, next <- rowsCombinations ns]
concatRow :: [Int] -> Int
concatRow = read . concatMap show

opToF :: Op -> (Int -> Int -> Int)
opToF Add = (+)
opToF Times = (*)
opToF Subtract = (-)
-- this is filth
opToF Divide = (\a b -> if a `mod` b == 0 then a `div` b else 0)
evaluateRange :: Op -> [[Int]] -> [Int]
evaluateRange op (x:xs) = [
    foldl (opToF op) (concatRow x) (map concatRow xs),
    foldr (opToF op) (concatRow x) (map concatRow xs)]

rangeFills :: KenKenRange -> [[[Int]]]
rangeFills (KenKenRange op target rowLengths) =
  filter ((target `elem`) . evaluateRange op) $ rowsCombinations (map snd rowLengths)

-------

emptyKenKen = fromList 9 9 (replicate 81 Nothing)

placePiece :: PartialKenKen -> KenKenPiece -> [[Int]] -> PartialKenKen
placePiece m ((r, c), KenKenRange _ _ rs) ns =
  let
    makeCoordsForRow rDelta (offset, n) = [(r+rDelta, c+cDelta) | cDelta <- [offset..(offset+n-1)]]
    coordsToPlace = concatMap (uncurry makeCoordsForRow) $ zip [0..] rs
    nsToPlace = concat ns
    toPlace = zip coordsToPlace nsToPlace
  in
    foldl (\m' ((r, c), n) -> setElem (Just n) (r, c) m') m toPlace

placedRows = map catMaybes . toLists
placedCols = placedRows . transpose
hasDup xs = nub xs /= xs
isPartialValid :: PartialKenKen -> Bool
isPartialValid m =
  all (not . hasDup) (placedRows m) && all (not . hasDup) (placedCols m)
placePieces =
  foldl (\ms (p, cs) -> filter isPartialValid [placePiece m p c | m <- ms, c <- cs]) [emptyKenKen]

solve :: KenKen -> [PartialKenKen]
solve (KenKen ps) =
  let
    calculateCombinations = zip ps $ map (rangeFills . snd) ps
    byLeastFlexible = sortBy (compare `on` (length . snd)) calculateCombinations
  in
    placePieces byLeastFlexible

-------

range1 = KenKenRange Divide 21 [(0, 2), (0, 1)]
piece1 = ((1, 1), range1)
range2 = KenKenRange Times 1960 [(0, 3), (2, 1)]
piece2 = ((1, 3), range2)
range3 = KenKenRange Divide 13 [(0, 2), (0, 1)]
piece3 = ((1, 6), range3)
range4 = KenKenRange Subtract 17 [(0, 2), (1, 1)]
piece4 = ((1, 8), range4)
range5 = KenKenRange Divide 5 [(0, 3), (1, 3)]
piece5 = ((2, 2), range5)
range6 = KenKenRange Times 969 [(0, 2), (1, 2)]
piece6 = ((2, 7), range6)
range7 = KenKenRange Divide 21 [(0, 2), (0, 1)]
piece7 = ((3, 1), range7)
range8 = KenKenRange Subtract 66 [(1, 1), (0, 2)]
piece8 = ((3, 5), range8)
range9 = KenKenRange Subtract 1 [(0, 1), (0, 1)]
piece9 = ((3, 7), range9)
range10 = KenKenRange Add 63 [(1, 2), (0, 2)]
piece10 = ((4, 1), range10)
range11 = KenKenRange Times 342 [(1, 1), (0, 2), (1, 1)]
piece11 = ((4, 3), range11)
range12 = KenKenRange Divide 9 [(0, 2), (0, 1)]
piece12 = ((4, 8), range12)
range13 = KenKenRange Times 59049 [(0, 3), (2, 1), (1, 2)]
piece13 = ((5, 5), range13)
range14 = KenKenRange Subtract 73 [(0, 3), (0, 3)]
piece14 = ((6, 1), range14)
range15 = KenKenRange Subtract 1 [(1, 2), (0, 2)]
piece15 = ((6, 4), range15)
range16 = KenKenRange Add 57 [(1, 1), (0, 2), (1, 1)]
piece16 = ((5, 8), range16)
range17 = KenKenRange Subtract 22 [(0, 1), (0, 2)]
piece17 = ((7, 8), range17)
range18 = KenKenRange Add 66 [(0, 1), (0, 2)]
piece18 = ((8, 1), range18)
range19 = KenKenRange Divide 17 [(0, 3), (1, 1)]
piece19 = ((8, 2), range19)
range20 = KenKenRange Add 1009 [(1, 2), (0, 3)]
piece20 = ((8, 4), range20)
range21 = KenKenRange Times 1056 [(0, 1), (0, 3)]
piece21 = ((8, 7), range21)

kenken = KenKen [
  piece1,
  piece2,
  piece3,
  piece4,
  piece5,
  piece6,
  piece7,
  piece8,
  piece9,
  piece10,
  piece11,
  piece12,
  piece13,
  piece14,
  piece15,
  piece16,
  piece17,
  piece18,
  piece19,
  piece20,
  piece21]

finalBoard = fmap fromJust $ head $ solve kenken

---

pieceToRows m ((r, c), KenKenRange _ _ rs) =
  map (\(rDelta, (offset, n)) -> concatRow [getElem (r + rDelta) (c + cDelta) m | cDelta <- [offset..(offset+n-1)]]) $ zip [0..] rs
pieceToMax m p = maximum (pieceToRows m p)
solve2 m (KenKen ps) =
  sum $ map (pieceToMax m) ps

answer = solve2 finalBoard kenken

main = print answer
