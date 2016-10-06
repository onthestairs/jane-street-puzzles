module Main where

import Data.Matrix
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Vector as Vector

type Value = Maybe Int
instance Show Value where
  show x = case x of
    Just x -> show x
    otherwise -> "."
type Grid = Matrix Value
type Depth = Int
type Position = (Int, Int)
type Move = (Position -> Position)
type DepthLookup = Depth -> Maybe Position
type LengthChecksum = Map.Map Int Int

----
flipReverse :: Ord y =>  Map.Map x y -> Map.Map y x
flipReverse m = Map.fromList (map (\(x, y) -> (y, x)) (Map.toList m))
----

setGridInt :: Grid -> Position -> Int -> Grid
setGridInt grid (x, y) n = setElem (Just n) (y, x) grid

gridValue :: Grid -> Position -> Value
gridValue grid (x, y) = getElem y x grid

isOnGrid :: Grid -> Position -> Bool
isOnGrid grid (x, y) =
  case safeGet y x grid of
    Just x -> True
    Nothing -> False

matchesGrid :: Grid -> Position -> Depth -> Bool
matchesGrid grid position depth =
  case gridValue grid position of
    Just x -> x == depth
    Nothing -> True

allMoves :: [Move] -- might be fun to refactor to auto-gen
allMoves =
  [(\(x, y) -> (x+1, y+2))
 , (\(x, y) -> (x+1, y-2))
 , (\(x, y) -> (x-1, y+2))
 , (\(x, y) -> (x-1, y-2))
 , (\(x, y) -> (x+2, y+1))
 , (\(x, y) -> (x+2, y-1))
 , (\(x, y) -> (x-2, y+1))
 , (\(x, y) -> (x-2, y-1))]

isValidMove :: Grid -> Depth -> Position -> DepthLookup -> Move -> Bool
isValidMove grid depth position depthLookup move =
  let
    newPosition = move position
    in (depth <= 8 || (lengthSum (Vector.toList (getCol 1 grid)) == 7)) &&
       (isOnGrid grid newPosition) && (matchesGrid grid newPosition depth) && (case depthLookup depth of
      Just startPosition -> startPosition == newPosition
      Nothing -> True) && (validCheckSums (<=) rowChecksums columnChecksums grid)

isComplete :: Depth -> Bool
isComplete depth = (depth == 28)

makeGrids :: DepthLookup -> Grid -> Depth -> Position -> [Grid]
makeGrids depthLookup grid depth position =
  let newGrid = setGridInt grid position depth
      completed = isComplete depth
      in case completed of
          True -> [newGrid]
          False -> let
            newDepth = depth + 1
            validMoves = filter (isValidMove newGrid newDepth position depthLookup) $ allMoves
            in concat $ map (\move -> makeGrids depthLookup newGrid newDepth (move position)) $ validMoves

rotateMatrix :: Matrix a -> Matrix a
rotateMatrix m = id m

hasRotationalSymmetry :: Grid -> Bool
hasRotationalSymmetry grid =
  let
    gridMask = fmap (\x -> case x of
      Just x -> True
      Nothing -> False) grid
    in all (== gridMask) $ take 4 $ (iterate rotateMatrix gridMask)


lengthSum xs = sum $ map (fromMaybe 0) xs

validCheckSums :: (Int -> Int -> Bool) -> LengthChecksum -> LengthChecksum -> Grid -> Bool
validCheckSums cmp rowChecksums columnChecksums grid =
  checkRows && checkColumns
  where
    rows = nrows grid
    columns = ncols grid
    doIndex checksums lengthGet index = case Map.lookup index checksums of
      Just checksum -> cmp (lengthSum (Vector.toList (lengthGet index grid))) checksum
      Nothing -> False
    checkRows = all (doIndex rowChecksums getRow) [1..rows]
    checkColumns = all (doIndex columnChecksums getCol) [1..columns]

----------- problem specific

startingPositionsMap :: Map.Map Position Int
startingPositionsMap = Map.fromList [((4,3), 11), ((6,4), 14), ((3,5), 8), ((5,6), 15)]

depthToStartingPosition = flipReverse startingPositionsMap

depthLookup :: DepthLookup
depthLookup depth = Map.lookup depth depthToStartingPosition

startingPositions :: [Position]
startingPositions =
  let
    size = 8
    allPositions = [(x,y) | x <- [1..size], y <- [1..size]]
    in filter (\position -> Map.notMember position startingPositionsMap) allPositions

rowChecksums :: LengthChecksum
rowChecksums = Map.fromList [
  (1, 10),
  (2, 34),
  (3, 108),
  (4, 67),
  (5, 63),
  (6, 84),
  (7, 24),
  (8, 16)]

columnChecksums :: LengthChecksum
columnChecksums = Map.fromList [
    (1, 7),
    (2, 14),
    (3, 72),
    (4, 66),
    (5, 102),
    (6, 90),
    (7, 42),
    (8, 13)]

startingGrid :: Grid
startingGrid =
  matrix 8 8 startLookup
  where
    startLookup (y, x) = Map.lookup (x, y) startingPositionsMap

-- filter (hasRotationalSymmetry . validCheckSums) $ concat $ map (makeGrids depthLookup startingGrid 1) startingPositions

-- main :: IO ()
-- main = someFunc
