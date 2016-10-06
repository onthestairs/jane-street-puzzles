module Main where

import Prelude hiding (Left, Right)
import Tetra
import Data.Matrix
import qualified Data.List as List

type Board a = Matrix a


getElem' (row, col) = getElem row col
add2Tuple (x, y) (x', y') = (x+x', y+y')

startGrey = "\x1b[90m"
startMegenta = "\x1b[35m"
startNormal = "\x1b[0m"
makeGrey s = startGrey ++ s ++ startNormal
makeMagenta s = startMegenta ++ s ++ startNormal

data Triangle a = Up a | Down a
instance (Show a) => Show (Triangle a) where
  show (Up x)   = (makeGrey "/") ++ show x ++ (makeGrey "\\")
  show (Down x) = (makeGrey "\\") ++ show x ++ (makeGrey "/")
getTriangleValue :: Triangle a -> a
getTriangleValue (Up   a) = a
getTriangleValue (Down a) = a

type Position = (Int, Int)
type PositionDelta = (Int, Int)
data TetraState a = TetraState {
    tetra :: (OrientedTetra a)
  , path :: [Position] -- most recently visited first
} deriving (Show)

mostRecentPosition :: TetraState a -> Position
mostRecentPosition = head . path

moveToDelta :: (Triangle a) -> Move -> PositionDelta
moveToDelta (Up _) MoveLeft = (-1, -1)
moveToDelta (Up _) MoveRight = (-1, 0)
moveToDelta (Up _) MoveOppositePeak = (1, 0)
moveToDelta (Down _) MoveLeft = (1, 0)
moveToDelta (Down _) MoveRight = (1, 1)
moveToDelta (Down _) MoveOppositePeak = (-1, 0)

getNewPosition :: Board (Triangle a) -> Position -> Move -> Position
getNewPosition board position move =
  let currentTriangle = (getElem' position board)
      moveDelta = moveToDelta currentTriangle move
  in
    add2Tuple position moveDelta

isLegalMove :: (Eq a) => Board (Triangle a) -> TetraState a -> Move -> Bool
isLegalMove board tetraState move =
  let newPosition@(row, col) = getNewPosition board (mostRecentPosition tetraState) move
      newValue = getTriangleValue (getElem' newPosition board)
      notSeen move = not $ newPosition `elem` (path tetraState)
      isOnBoard move = (row >= 1 && row <= nrows board) && (col >= 1 && col <= ncols board)
      isMatchingFace move = (tetraFaceValue (roll move (tetra tetraState))) == newValue
  in
    notSeen move && isOnBoard move && isMatchingFace move

legalPosibleMoves :: (Eq a) => Board (Triangle a) -> TetraState a -> [TetraState a]
legalPosibleMoves board tetraState =
  let moveToState move = TetraState (roll move (tetra tetraState))
                                    ((getNewPosition board (mostRecentPosition tetraState) move):(path tetraState))
  in
    map moveToState $ filter (isLegalMove board tetraState) moves


findPaths :: (Eq a) => Board (Triangle a) -> TetraState a -> [TetraState a]
findPaths board tetra =
  let nextStates = legalPosibleMoves board tetra
      isFinished state = (fst $ mostRecentPosition state) == (nrows board)
      (finishedStates, pendingStates) = List.partition isFinished nextStates
  in (concatMap (findPaths board) nextStates) ++ finishedStates

----------------


triBoardLists :: [[Triangle Int]]
triBoardLists = [
   map Down [1,71,711,711,1,17,711,1]
 , map Up   [71,1,711,17,71,71,71,711]
 , map Down [711,71,17,71,17,711,711,71]
 , map Up   [71,17,711,1,17,711,17,1]
 , map Down [711,711,1,17,1,17,1,711]
 , map Up   [17,1,71,711,711,711,71,17]
 , map Down [71,71,711,71,711,17,711,1]
 , map Up   [1,1,17,71,17,1,71,71]
 , map Down [71,71,1,17,1,17,711,17]
 , map Up   [1,711,17,711,71,71,71,71]
 , map Down [711,71,1,1,71,71,711,1]
 , map Up   [71,1,17,711,711,1,17,71]
 , map Down [1,17,1,17,1,17,711,17]
 , map Up   [71,711,17,711,71,711,1,711]
 , map Down [711,17,71,1,17,71,17,1]
 , map Up   [1,1,711,711,71,711,711,17]]

triBoard = fromLists triBoardLists

-- seenPositions = Set.empty
startingTetra = DownTetra (Tetra 1 17 711 71) :: OrientedTetra Int
startingState = TetraState startingTetra [(1, 1)] :: TetraState Int

allTetras :: [OrientedTetra Int]
allTetras =
  let perms = List.permutations [1, 17, 71, 711]
      makeTetra [a, b, c, d] = DownTetra (Tetra a b c d)
  in map makeTetra perms

isLegalStart :: (Eq a) => Board (Triangle a) -> OrientedTetra a -> Position -> Bool
isLegalStart board tetra position =
  let boardValue = getTriangleValue (getElem' position board)
      tetraValue = tetraFaceValue tetra
  in boardValue == tetraValue

legalStartStates :: Board (Triangle Int) -> [OrientedTetra Int] -> [TetraState Int]
legalStartStates board tetras =
  let potentialStartingPositions = [(1, col) | col <- [1..(ncols board)]]
  in
   [
      (TetraState tetra [position]) |
      tetra <- tetras
    , position <- potentialStartingPositions
    , isLegalStart board tetra position
  ]


allPaths = concatMap (findPaths triBoard) (legalStartStates triBoard allTetras)
stateValue board (TetraState _ path) = sum $ map (\position -> getTriangleValue (getElem' position board)) path

solve = map (stateValue triBoard) allPaths

-------
-- Try to come up with a pretty print of a solution

fancyShowTriangle (Up x)   = (makeMagenta "/") ++ show x ++ (makeMagenta "\\")
fancyShowTriangle (Down x) = (makeMagenta "\\") ++ show x ++ (makeMagenta "/")

data FancyPathTriangle a = InPathTriangle (Triangle a) | OutPathTriangle (Triangle a)
fill k str = replicate (k - length str) ' ' ++ str
instance (Show a) => Show (FancyPathTriangle a) where
  show (InPathTriangle     (Up x)) = (makeGrey " /") ++ startMegenta ++ (fill 3 $ show x) ++ (makeGrey "\\ ")
  show (InPathTriangle   (Down x)) = (makeGrey " \\") ++ startMegenta ++ (fill 3 $ show x) ++ (makeGrey "/ ")
  show (OutPathTriangle    (Up x)) = (makeGrey " /") ++ (fill 3 $ show x) ++ (makeGrey "\\ ")
  show (OutPathTriangle  (Down x)) = (makeGrey " \\") ++ (fill 3 $ show x) ++ (makeGrey "/ ")

data WildBoard a = WildBoard (Board (FancyPathTriangle a))
instance (Show a) => Show (WildBoard a) where
  show b = wildBoard b

prettyState :: Board (Triangle a) -> TetraState a -> (Board (FancyPathTriangle a))
prettyState board (TetraState _ path) =
  (matrix (nrows board) (ncols board) makeCell)
  where
    makeCell (row, col) =
      let isInPath = (row, col) `elem` path
      in
        case isInPath of
        True  -> (InPathTriangle (getElem' (row, col) board))
        False -> (OutPathTriangle (getElem' (row, col) board))

chunk :: [a] -> [(a,a)]
chunk [] = []
chunk (x:y:zs) = (x, y):(chunk zs)

wildBoard :: (Show a) => WildBoard a -> String
wildBoard (WildBoard board) =
  let doubles = (zip [1..] (chunk $ toLists board))
      cellLength = 7
      doRow (i, (downRow, upRow)) =
        let
          indent =  (List.intercalate "") $ take (((length doubles) - i) * cellLength) $ repeat " "
          displayDouble = (List.intercalate "") $ [show up ++ show down | (down, up) <- zip downRow upRow] :: String
        in
          indent ++ displayDouble :: String
  in
    unlines $ map doRow doubles
