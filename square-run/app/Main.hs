{-# LANGUAGE TemplateHaskell #-}

module Main where

import Linear.V2
import Data.Function.Memoize
import Control.Applicative
import Data.Ord (comparing)
import Data.Foldable (maximumBy)
import Data.Bifunctor
import Data.List (intercalate)

type Board = [[Int]]
type Coord = V2 Int
type Move = Coord -> Coord

deriveMemoizable ''V2

getElem' :: Board -> Coord -> Int
getElem' m (V2 row col) = (m !! (row - 1)) !! (col - 1)

boardMap :: (Int -> Int) -> Board -> Board
boardMap f cols =
  map (\row -> map f row) cols

inbounds :: Coord -> Bool
inbounds (V2 row col) = row <= 8 && row >= 1 && col <= 8 && col >= 1

allQueenDeltas :: [Coord]
allQueenDeltas = [V2 row col | row <- [-8..8], col <- [-8..8], abs(row) == abs(col) || row == 0 || col == 0, not (row == 0 && col == 0)]

allQueenMoves :: [Move]
allQueenMoves = map (\d -> (+) d) allQueenDeltas

queenMoves :: Coord -> [Move]
queenMoves c = filter (\m -> inbounds (m c)) allQueenMoves

queenPaths :: Coord -> [[Coord]]
queenPaths c = [c:cs | m <- queenMoves c, cs <- queenPaths (m c)]

------------

isPerfectSquare n = n `elem` [i*i | i <- [1..n]]

data ReductionType = ByOne | ByFive deriving (Show)

coordReductionType :: Board -> Coord -> Coord -> ReductionType
coordReductionType b start end =
  if isPerfectSquare $ (getElem' b start + getElem' b end) then ByOne else ByFive

reduceBoard :: Board -> ReductionType -> Board
reduceBoard b ByOne = boardMap (\x -> x - 1) b
reduceBoard b ByFive = boardMap (\x -> x - 5) b

reduceBoardAfterMove :: Board -> Coord -> Coord -> Board
reduceBoardAfterMove b start end = reduceBoard b (coordReductionType b start end)

--------------

pathBoards :: Board -> [Coord] -> [Board]
pathBoards b cs =
  scanl (\b' (start, end) -> reduceBoardAfterMove b' start end) b joins
  where joins = zip cs (tail cs)

pathScores :: Board -> [Coord] -> [Int]
pathScores b cs = map (uncurry getElem') (zip (pathBoards b cs) (tail cs))

pathTotalScores :: Board -> [Coord] -> Int
pathTotalScores b cs = sum $ pathScores b cs

---------------

isGoodBoard :: Board -> Bool
isGoodBoard b =
  any (> (-20)) (concat b)

type CurrentSpace = Maybe (Int, [Coord])
type EndSpace = Maybe (Int, [Coord])
type SpaceAnswer = Maybe (Int, [Coord])

solve :: (Board -> Coord -> SpaceAnswer) -> Board -> Coord -> SpaceAnswer
solve f b c =
  maximumBy (comparing (fmap fst)) $ map next (queenMoves c)
  where
    next :: Move -> SpaceAnswer
    next m = best
      where nextCoord = m c
            nextBoard = reduceBoardAfterMove b c nextCoord
            value = getElem' b nextCoord
            isGoodNextBoard = isGoodBoard nextBoard
            nextAnswer = f nextBoard nextCoord
            maxNext = if isGoodNextBoard then (bimap (+ value) ((:) c)) <$> nextAnswer else Nothing
            final = if nextCoord == endCoord then Just (value, [c, endCoord]) else Nothing
            best = maximumBy (comparing (fmap fst)) $ [final, maxNext]

solve' = memoFix2 solve

examplePath :: [Coord]
examplePath = [V2 8 1, V2 4 1, V2 6 1, V2 6 2, V2 8 4, V2 8 1, V2 5 4, V2 1 8, V2 8 1, V2 1 8]

startCoord = V2 8 1
endCoord = V2 1 8

board :: Board
board = [
  [ 8,  5, 13, 23, 29, 15, 23, 30],
  [17, 22, 30,  3, 13, 25,  2, 14],
  [10, 15, 18, 28,  2, 18, 27,  6],
  [ 0, 31,  1, 11, 22,  7, 16, 20],
  [12, 17, 24, 26,  3, 24, 25,  5],
  [27, 31,  8, 11, 19,  4, 12, 21],
  [21, 20, 28,  4,  9, 26,  7, 14],
  [ 1,  6,  9, 19, 29, 10, 16,  0]]

prettyCoord :: Coord -> String
prettyCoord (V2 row col) =
  (['a'..'h'] !! (col - 1)) : show (9 - row)

prettyPath :: [Coord] -> String
prettyPath cs = intercalate " -> " (map prettyCoord cs)

main :: IO ()
main = putStrLn ("Max score: " ++ show v ++ ", Path: " ++ prettyPath cs)
  where Just (v, cs) = solve' board startCoord
