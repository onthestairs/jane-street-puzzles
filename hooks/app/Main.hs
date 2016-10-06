module Main where

import Data.Matrix
import qualified Data.Vector as Vector
import qualified Data.List as List

data Position = NE | SE | SW | NW deriving (Show)
type Placements = [Position]
type Mask = Matrix Int
data Cell = Cell Int Bool
type Grid = Matrix Cell
type GridOn = Matrix Int
type CheckSum = [Int]

instance Show Cell where
  show (Cell x on) = if on then "(" ++ show x ++ ")" else show x

potentialPlacements :: Int -> [Placements]
potentialPlacements n = potentialPlacements' (n-1)
  where potentialPlacements' 0 = [[]]
        potentialPlacements' n = (:) <$> [NE, SE, SW, NW] <*> potentialPlacements' (n-1)

placementsToMask :: Placements -> Mask
placementsToMask [] = fromLists [[1]]
placementsToMask (p:ps) =
  let n = (length (p:ps)) + 1
      horizontal = fromLists [take n $ repeat n]
      vertical = transpose $ fromLists [take (n-1) $ repeat n]
      subMask = placementsToMask ps
  in case p of
    NE -> horizontal <-> (subMask <|> vertical)
    SE -> (subMask <|> vertical) <-> horizontal
    SW -> (vertical <|> subMask) <-> horizontal
    NW -> horizontal <-> (vertical <|> subMask)

makeCovers :: Int -> [[Bool]]
makeCovers 0 = [[]]
makeCovers n = (:) <$> [True, False] <*> makeCovers (n-1)

equalCovers ::  [Int] -> Int -> [[Bool]]
equalCovers xs target =
  filter equalCover $ makeCovers (length xs)
  where equalCover cover = coverSum cover == target
        coverSum cover = sum (map fst (filter ((== True) . snd) (zip xs cover)))

maskResults :: CheckSum -> CheckSum -> Mask -> [Grid]
maskResults rowCheckSum colCheckSum mask =
  let emptyGrid = fmap (\x -> Cell x False) mask
  in doRow rowCheckSum colCheckSum 1 emptyGrid

turnGridOn :: Grid -> GridOn
turnGridOn = fmap (\(Cell x on) -> if on then x else 0)

toMask :: Grid -> Mask
toMask = fmap (\(Cell x _) -> x)

validCols :: CheckSum -> Int -> Grid -> Bool
validCols colCheckSum row grid =
  let colSums = map sum (toLists (transpose (turnGridOn grid)))
      n = nrows grid
      lessThan (a, b) = a <= b
      greaterThan (a, b) = a >= (b - (sum [n,(n-1)..row]))
  in and ([lessThan, greaterThan] <*> (zip colSums colCheckSum))

applyCover :: Grid -> Int -> [Bool] -> Grid
applyCover grid row cover =
  foldr (\(col, on) grid' -> let (Cell x _) = grid ! (row, col) in setElem (Cell x on) (row, col) grid')
        grid
        (zip [1..] cover)

doRow :: CheckSum -> CheckSum -> Int -> Grid -> [Grid]
doRow rowCheckSum colCheckSum row grid =
  let rowVals = Vector.toList $ getRow row (toMask grid)
      checkSum = rowCheckSum !! (row - 1)
      covers = equalCovers rowVals checkSum
      -- covers = equalCovers [4,4,4,4] checkSum
      -- covers = makeCovers (length rowVals)
      newGrids = filter (validCols colCheckSum row) $ map (applyCover grid row) covers
      -- newGrids = map (applyCover grid row) covers
  in
    if row == nrows grid
    then newGrids
    -- else newGrids
    else concatMap (doRow rowCheckSum colCheckSum (row + 1)) newGrids

allResults :: CheckSum -> CheckSum -> Int -> [Grid]
allResults rowCheckSum colCheckSum n = concatMap (maskResults' . placementsToMask) (potentialPlacements n)
  where maskResults' = maskResults rowCheckSum colCheckSum

--------------------

solution =
 fromLists [[Cell 9 True, Cell 9 False, Cell 9 True, Cell 9 False, Cell 9 False, Cell 9 True, Cell 9 False, Cell 9 True, Cell 9 True],
            [Cell 6 False, Cell 5 False, Cell 5 True, Cell 5 True, Cell 5 True, Cell 5 True, Cell 7 True, Cell 8 True, Cell 9 True],
            [Cell 6 False, Cell 5 False, Cell 4 False, Cell 4 True, Cell 4 False, Cell 4 False, Cell 7 False, Cell 8 False, Cell 9 False],
            [Cell 6 True, Cell 5 True, Cell 4 True, Cell 3 True, Cell 3 True, Cell 3 True, Cell 7 True, Cell 8 True, Cell 9 True],
            [Cell 6 False, Cell 5 False, Cell 4 True, Cell 2 True, Cell 1 True, Cell 3 False, Cell 7 False, Cell 8 False, Cell 9 False],
            [Cell 6 False, Cell 5 False, Cell 4 True, Cell 2 False, Cell 2 True, Cell 3 False, Cell 7 False, Cell 8 True, Cell 9 False],
            [Cell 6 True, Cell 6 False, Cell 6 True, Cell 6 True, Cell 6 True, Cell 6 True, Cell 7 False, Cell 8 True, Cell 9 True],
            [Cell 7 True, Cell 7 False, Cell 7 True, Cell 7 True, Cell 7 False, Cell 7 True, Cell 7 True, Cell 8 True, Cell 9 False],
            [Cell 8 True, Cell 8 False, Cell 8 True, Cell 8 True, Cell 8 False, Cell 8 False, Cell 8 False, Cell 8 False, Cell 9 True]] :: Grid


isValidPerm :: Grid -> [Int] -> Bool
isValidPerm grid permutation =
  all (\(row, col) -> let (Cell _ on) = grid ! (row, col) in on) $ zip permutation [1..]

makeProduct :: Grid -> [Int] -> Int
makeProduct grid permutation =
  product $ map (\(row, col) -> let (Cell x _) = grid ! (row, col) in x) $ zip permutation [1..]

makeProducts :: Grid -> [(Int, [Int])]
makeProducts grid =
  let permutations = List.permutations [1..nrows grid]
      validPerms = filter (isValidPerm grid) permutations
  in zip (map (makeProduct grid) validPerms) validPerms

maxProduct :: Grid -> (Int, [Int])
maxProduct grid = maximum $ makeProducts grid

----------

testRowChecksum = [7, 11, 4, 8] :: CheckSum
testColChecksum = [10, 3, 5, 12] :: CheckSum
testN = 4 :: Int
-- main = putStr $ show $ take 1 $ allResults testRowChecksum testColChecksum testN

realRowChecksum = [45, 44, 4, 48, 7, 14, 47, 43, 33] :: CheckSum
realColChecksum = [36, 5, 47, 35, 17, 30, 21, 49, 45] :: CheckSum
realN = 9 :: Int
main = putStr $ show $ allResults realRowChecksum realColChecksum realN
