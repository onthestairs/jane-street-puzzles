module Main where

import Data.Matrix
import qualified Data.Map as Map

type Position = (Int, Int)
type Value = Maybe Int
type Path = [Position]
data DiceLabel = A | B | C | D | E| F deriving (Show, Eq, Ord)
data DiceFace = DiceFace Value DiceLabel deriving (Show, Eq)
type Dice = (DiceFace, DiceFace, DiceFace, DiceFace, DiceFace, DiceFace)
data Status = PENDING | FINISHED | CONTRADICTION deriving (Show, Eq)
data DicePath = DicePath {
  path :: Path,
  dice :: Dice,
  status :: Status,
  labelPath :: [DiceLabel]
} deriving (Show, Eq)
data Move = UP | RIGHT | DOWN | LEFT
type Grid = Matrix Value

moveDice :: Move -> Dice -> Dice
moveDice UP (top, north, east, south, west, bottom) = (south, top, east, bottom, west, north)
moveDice RIGHT (top, north, east, south, west, bottom) = (west, north, top, south, bottom, east)
moveDice LEFT dice = (moveDice RIGHT) . (moveDice RIGHT) . (moveDice RIGHT) $ dice
moveDice DOWN dice = (moveDice UP) . (moveDice UP) . (moveDice UP) $ dice

topDice :: Dice -> DiceFace
topDice (top, _, _, _, _, _) = top

positionValue :: Grid -> Position -> Value
positionValue grid (x, y) =
  getElem y x grid

positionNumber :: Grid -> Position -> Int
positionNumber grid (x, y) = let value = positionValue grid (x, y) in
  case value of
    Nothing -> 9
    Just v -> v

diceValueLookup :: Dice -> Map.Map DiceLabel Integer
diceValueLookup (t, n, e, s, w, b) =
  let diceList = [t, n, e, s, w, b]
      getPair (DiceFace (Just value) label) = (label, toInteger value)
      getPair (DiceFace Nothing label) = (label, 9)
      labelPairs = map getPair diceList
      labelMap = Map.fromList labelPairs
  in labelMap

completeDice :: Dice -> Dice
completeDice (t, n, e, s, w, b) =
  (makeNine t, makeNine n, makeNine e, makeNine s, makeNine w, makeNine b)
  where makeNine (DiceFace Nothing label) = DiceFace (Just 9) label
        makeNine (DiceFace value label) = DiceFace value label

pathValue :: DicePath -> Integer
pathValue dicePath =
  let currentLabelPath = labelPath dicePath
      currentDice = dice dicePath
      completedDice = completeDice currentDice
      diceLabelMap = diceValueLookup currentDice
      lookupValue label = case Map.lookup label diceLabelMap of
        Just value -> value
        Nothing -> 9
  in foldr (*) 1 (map lookupValue currentLabelPath)

positionMove :: Move -> Position -> Position
positionMove move (x, y) =
  case move of
    DOWN -> (x, y+1)
    RIGHT -> (x+1, y)
    UP -> (x, y-1)
    LEFT -> (x-1, y)

possibleMoves :: DicePath -> Grid -> [Move]
possibleMoves dicePath grid = filter canDo [DOWN, RIGHT, LEFT, UP]
  where currentPath = path dicePath
        gridSize = ncols grid
        (x, y) = last currentPath
        canDo move = let (newX, newY) = positionMove move (x, y)
          in (not $ (newX, newY) `elem` currentPath) && (newX <= gridSize && newX >= 1) && (newY <= gridSize && newY >= 1)

updateDiceTop :: Dice -> Value -> Dice
updateDiceTop ((DiceFace _ topLabel), north, east, south, west, bottom) value =
  ((DiceFace value topLabel), north, east, south, west, bottom)

assignDice :: Dice -> Grid -> Position -> Dice
assignDice dice grid position =
  let
    (DiceFace topValue label) = topDice dice
    gridValue = positionValue grid position
    newDice = case topValue of
      Nothing -> updateDiceTop dice gridValue
      otherwise -> dice
    in newDice

doMove :: DicePath -> Grid -> Move -> DicePath
doMove dicePath grid move =
  let (x, y) = last (path dicePath)
      gridSize = ncols grid
      newPosition = positionMove move (x, y)
      newDice = moveDice move (dice dicePath)
      newPath = path dicePath ++ [newPosition]
      assignedDice = assignDice newDice grid newPosition
      DiceFace topValue label = topDice assignedDice
      gridValue = positionValue grid newPosition
      currentlabelPath = labelPath dicePath
      newLabelPath = currentlabelPath ++ [label]
      newStatus = if (gridValue /= Nothing) && topValue /= gridValue then
        CONTRADICTION
        else
          if newPosition == (gridSize, gridSize)
          then FINISHED
          else PENDING
  in
    dicePath {
      path = newPath,
      dice = assignedDice,
      status = newStatus,
      labelPath = newLabelPath
    }

createPaths :: Grid -> DicePath -> [DicePath]
createPaths grid dicePath =
  let moves = possibleMoves dicePath grid
      newdicePaths = map (doMove dicePath grid) moves
  in
      concat (map doNext (filter ((/= CONTRADICTION) . status) newdicePaths))
      where
        doNext newDicePath = case status newDicePath of
          FINISHED -> [newDicePath]
          PENDING -> createPaths grid newDicePath

simpleDice :: Dice
-- simpleDice = (
--   DiceFace (Just 3) A, --top
--   DiceFace (Just 4) B, --north
--   DiceFace (Just 8) C, --east
--   DiceFace (Just 5) D, --south
--   DiceFace Nothing E, --west
--   DiceFace (Just 1) F) --bottom
simpleDice = (
    DiceFace (Just 3) A, --top
    DiceFace Nothing B, --north
    DiceFace Nothing C, --east
    DiceFace Nothing D, --south
    DiceFace Nothing E, --west
    DiceFace Nothing F) --bottom

-- simpleGrid :: Grid
-- simpleGrid = map (\x -> (GridValue x Nothing)) $ fromLists [[Just 3, Just 4, Just 1, Just 7, Just 5]
--               , [Just 1, Just 2, Just 4, Just 3, Just 5]
--               , [Just 2, Just 4, Just 3, Just 6, Just 2]
--               , [Just 9, Just 5, Just 7, Just 2, Just 3]
--               , [Just 5, Just 8, Just 3, Just 4, Just 1]]
--
--
--
simpleDicePath = DicePath {
  path = [(1, 1)],
  dice = simpleDice,
  status = PENDING,
  labelPath = [A]
}

constructGrid :: [[Value]] -> Grid
constructGrid gridLists =
  fromLists gridLists

x :: [[Value]]
x = [[Just 3, Nothing, Just 1, Just 7, Just 5]
    , [Just 1, Just 2, Just 4, Just 3, Just 5]
    , [Just 2, Just 4, Just 3, Just 6, Just 2]
    , [Just 9, Just 5, Just 7, Just 2, Just 3]
    , [Just 5, Just 8, Just 3, Just 4, Just 1]]

simpleGrid = constructGrid x

actualGridValues :: [[Value]]
actualGridValues = [[Just 1, Just 5, Just 4, Just 4, Just 6, Just 1, Just 1, Just 4, Just 1, Just 3, Just 7, Just 5],
                  [Just 3, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just 1],
                  [Just 4, Nothing, Just 6, Just 4, Just 1, Just 8, Just 1, Just 4, Just 2, Just 1, Nothing, Just 3],
                  [Just 7, Nothing, Just 1, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just 1, Nothing, Just 2],
                  [Just 1, Nothing, Just 1, Nothing, Just 6, Just 1, Just 6, Just 2, Nothing, Just 2, Nothing, Just 1],
                  [Just 8, Nothing, Just 4, Nothing, Just 1, Nothing, Nothing, Just 8, Nothing, Just 3, Nothing, Just 5],
                  [Just 4, Nothing, Just 2, Nothing, Just 5, Nothing, Nothing, Just 3, Nothing, Just 5, Nothing, Just 2],
                  [Just 8, Nothing, Just 5, Nothing, Just 1, Just 1, Just 2, Just 3, Nothing, Just 4, Nothing, Just 6],
                  [Just 6, Nothing, Just 1, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just 3, Nothing, Just 6],
                  [Just 3, Nothing, Just 6, Just 3, Just 6, Just 5, Just 4, Just 3, Just 4, Just 5, Nothing, Just 1],
                  [Just 6, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just 3],
                  [Just 2, Just 1, Just 6, Just 6, Just 4, Just 5, Just 2, Just 1, Just 1, Just 1, Just 7, Just 1]]

actualGrid = constructGrid actualGridValues

actualDice = (
    DiceFace (Just 1) A, --top
    DiceFace Nothing B, --north
    DiceFace Nothing C, --east
    DiceFace Nothing D, --south
    DiceFace Nothing E, --west
    DiceFace Nothing F) --bottom

actualDicePath = DicePath {
  path = [(1, 1)],
  dice = actualDice,
  status = PENDING,
  labelPath = [A]
}

main = let paths = (createPaths actualGrid actualDicePath)
          --  pairs = zip (map pathValue paths) paths
           max = maximum . (map pathValue) $ paths
       in putStrLn (show max)

-- pathValue $ head $  createPaths  simpleDicePath

-- solve = maximum . (pathValue simpleGrid) . allPaths
--
-- moveDi
