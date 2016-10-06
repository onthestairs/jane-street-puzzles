module Main where

import Data.Maybe
import Data.Matrix
import Data.Ord
import qualified Debug.Trace
import qualified Data.List as List
import qualified Data.Map.Lazy as Map
import qualified Data.IntMultiSet as IntMultiSet

data Piece = King | Queen | Knight | Bishop | Rook
instance Show Piece where
  show King = "K"
  show Queen = "Q"
  show Knight = "N"
  show Bishop = "B"
  show Rook = "R"
type Turn = Int
data PlacedPiece = PlacedPiece Turn Piece
instance Show PlacedPiece where
  show (PlacedPiece n p) = show p ++ " " ++ show n
type Square = Maybe PlacedPiece
type Board = Matrix Square
type PieceMove = (Int, Int) -- (RowDelta, ColDelta)
type Position = (Int, Int) -- (Row, Col)
type Decomposition = IntMultiSet.IntMultiSet
data Checksums = Checksums [[Decomposition]] [[Decomposition]] deriving Show

---------------------

pieceMoves :: Piece -> [PieceMove]
pieceMoves King = [(r, c) | r <- [1, -0, -1], c <- [1, 0, -1], (r, c) /= (0, 0)]
pieceMoves Queen = (pieceMoves Bishop) ++ (pieceMoves Rook)
pieceMoves Knight = [(1, -2), (1, 2), (-1, -2), (-1, 2), (2, -1), (2, 1), (-2, 1), (-2, -1)]
pieceMoves Rook = [(n, 0) | n <- [-8..8], n /= 0] ++ [(0, n) | n <- [-8..8], n /= 0]
pieceMoves Bishop = [(f n, g n) | n <- [1..8], f <- [id, negate], g <- [id, negate]]

getElem' :: (Int, Int) -> Matrix a -> a
getElem' = uncurry getElem

newPosition :: Position -> PieceMove -> Position
newPosition (row, col) (rowDelta, colDelta) =
  (row + rowDelta, col + colDelta)

isUnoccupied :: Position -> Board -> PieceMove -> Bool
isUnoccupied position board move =
  isNothing $ getElem' (newPosition position move) board

isOnBoard :: Position -> Board -> PieceMove -> Bool
isOnBoard position board move =
  let
    (newRow, newCol) = newPosition position move
  in
    (newRow <= 8 && newRow >= 1) && (newCol <= 8 && newCol >= 1)

isLegalMove :: Position -> Board -> PieceMove -> Bool
isLegalMove position board move = (isOnBoard position board move) && (isUnoccupied position board move)

legalMoves :: Position -> Board -> [PieceMove]
legalMoves position board =
  let (Just (PlacedPiece _ p)) = getElem' position board
      potentialMoves = pieceMoves p
  in
    filter (isLegalMove position board) potentialMoves

onBoardMoves :: Position -> Board -> [PieceMove]
onBoardMoves position board =
  let (Just (PlacedPiece _ p)) = getElem' position board
      potentialMoves = pieceMoves p
  in
    filter (isOnBoard position board) potentialMoves

applyMove :: Position -> Board -> PieceMove -> Board
applyMove position board move =
  let
    (Just (PlacedPiece n piece)) = getElem' position board
  in
    setElem (Just (PlacedPiece (n+1) piece)) (newPosition position move) board

----------

needsClearPath :: Piece -> Bool
needsClearPath King = True
needsClearPath Queen = True
needsClearPath Knight = False
needsClearPath Bishop = True
needsClearPath Rook = True

innerRange n = if n < 0
  then [(n+1)..(-1)]
  else [1..(n-1)]

pathBetween :: Position -> PieceMove -> [Position]
pathBetween (row, col) (0, colD) = [(row, col + d) | d <- innerRange colD]
pathBetween (row, col) (rowD, 0) = [(row + d, col) | d <- innerRange rowD]
pathBetween (row, col) (rowD, colD) = [
  (row + d1, col + d2) |
  (d1, d2) <- zip (innerRange rowD) (innerRange colD)]

isClearPath :: Position -> Board -> PieceMove -> Bool
isClearPath (row, col) board (rowD, colD) =
  let pathBetween' = pathBetween (row, col) (rowD, colD)
  in all isNothing (map (\(row', col') -> getElem row' col' board) pathBetween')

isAttacking :: Board -> Position -> Bool
isAttacking board position =
  let (Just (PlacedPiece _ p)) = getElem' position board
      moves = (onBoardMoves position board)
      isAttackingAPiece move = let targetSquare = getElem' (newPosition position move) board
                                   isPotentialVictim = isJust targetSquare
                                   isVictim = if needsClearPath p then
                                                isPotentialVictim && (isClearPath position board move)
                                              else
                                                isPotentialVictim
                               in
                                 isPotentialVictim
                                          -- isVictim
  in
    any isAttackingAPiece moves

findActivePositions :: Turn -> Board -> [Position]
findActivePositions turn board =
  let
    isActive Nothing = False
    isActive (Just (PlacedPiece n _)) = n == turn
  in [(r, c) | r <- [1..8], c <- [1..8], isActive (getElem r c board)]

makeTurnBoard :: Turn -> Board -> Board
makeTurnBoard turn board =
  let
    f Nothing = Nothing
    f square@(Just (PlacedPiece n _)) = if n == turn then square else Nothing
  in fmap f board

isNotUnderAttack :: Turn -> Board -> Bool
isNotUnderAttack turn board =
  let
      activePositions = findActivePositions turn board
      turnBoard = makeTurnBoard turn board
  in not $ any (isAttacking turnBoard) activePositions

---------------

squareValue :: Square -> Int
squareValue Nothing = 1
squareValue (Just (PlacedPiece 0 _)) = 1
squareValue (Just (PlacedPiece n _)) = n

-- rowMultiset :: Board -> [IntMultiSet.IntMultiSet]
-- rowMultiset board =
--   map (IntMultiSet.fromList . (map squareValue) . (filter isJust)) (toLists board)
--
-- colMultiset :: Board -> [IntMultiSet.IntMultiSet]
-- colMultiset = rowMultiset . transpose

isLineRealistic :: Turn -> [Square] -> [Decomposition] -> Bool
isLineRealistic turn row decompositions =
  let
      rowSet = IntMultiSet.fromList $ (map squareValue) $ (filter isJust row)
      rowWithoutOnes = IntMultiSet.deleteAll 1 rowSet
      isSubset decomposition = (IntMultiSet.isSubsetOf rowWithoutOnes decomposition)
      missingFactors decomposition = IntMultiSet.difference decomposition rowWithoutOnes
      hasNoSmallNumbersLeft decomposition = all (> turn) (IntMultiSet.toList (missingFactors decomposition))
      spacesLeft = length $ filter isNothing row
      hasEnoughSpaces decomposition = spacesLeft >= (IntMultiSet.size (missingFactors decomposition))
      validDecompositions = filter (\c ->    isSubset c
                                          && hasNoSmallNumbersLeft c
                                          && hasEnoughSpaces c
                              ) decompositions
  in
    not $ null validDecompositions

isChecksumRealistic :: Checksums -> Turn -> Board -> Bool
isChecksumRealistic (Checksums rowChecksum7 colChecksum7) turn board =
  let rows = toLists board
      cols = (toLists . transpose) board
      validRow = all (\(s1, s2) -> isLineRealistic turn s1 s2) (zip rows rowChecksum7)
      validCol = all (\(s1, s2) -> isLineRealistic turn s1 s2) (zip cols colChecksum7)
  in
    validRow && validCol

---------------


isLegalTurn :: Checksums -> Turn -> Board -> Bool
isLegalTurn checksums turn board = (isChecksumRealistic checksums turn board) && (isNotUnderAttack turn board)

getActivePositions :: Board -> Turn -> [Position]
getActivePositions board turn =
  let
    takenPositions = filter (\position -> isJust $ getElem' position board) [(r, c) | r <- [1..8], c <- [1..8]]
    isActive (Just (PlacedPiece n _)) = n == (turn - 1)
  in
    filter (\position -> isActive $ getElem' position board) takenPositions

positionMoves :: Position -> Board -> [Board]
positionMoves position board =
  map (applyMove position board) (legalMoves position board)

doTurn :: Board -> Turn -> [Board]
doTurn board turn =
  let activePositions = getActivePositions board turn
      possibleEndBoards = foldl (\boards position -> concatMap (positionMoves position) boards) [board] activePositions
  in possibleEndBoards

doTurnWithChecks :: Board -> Turn -> Checksums -> [Board]
doTurnWithChecks board turn checksums =
  let boards = doTurn board turn
  in
    filter (isLegalTurn checksums turn) boards

run7 :: Board -> Checksums -> [Board]
run7 board checksums =
  foldl (\boards turn -> Debug.Trace.trace (show $ length boards)
                         (concatMap (\board' -> doTurnWithChecks board' turn checksums) boards)) [board] [1..7]

sumOfProducts :: Board -> Int
sumOfProducts board =
  let rowProducts board' = map (product . (map squareValue)) (toLists board')
      colProducts board' = rowProducts (transpose board')
  in (sum (rowProducts board)) + (sum (colProducts board))

run8th :: Board -> [(Int, Board)]
run8th board =
  let nextBoards = doTurn board 8
      legalNextBoards = filter (isNotUnderAttack 8) nextBoards
  in
    take 4 $ reverse $ (List.sortBy (comparing fst)) $ map (\board -> (sumOfProducts board, board)) legalNextBoards


run :: Board -> Checksums -> [(Int, Board)]
run board checksums =
  let board7 = head $ run7 board checksums
  in run8th board7

-------------
makeDecompositions :: Int -> [[Int]]
makeDecompositions 1 = [[]]
makeDecompositions n =
    [(m:xs) | m <- [2..7], n `mod` m == 0, xs <- makeDecompositions (n `div` m)]

makeLegalDecompositions :: Int -> [Decomposition]
makeLegalDecompositions = (List.nub) . (map IntMultiSet.fromList) . (filter ((<= 8) . length)) . makeDecompositions

------------

prettyBoard :: Board -> Matrix String
prettyBoard board =
  let
      f Nothing = "."
      f (Just (PlacedPiece n p)) = show n ++ show p
  in fmap f board

startingBoard =
  matrix 8 8 positionValue
  where
    positionValue position = case Map.lookup position startingPositions of
                               Nothing -> Nothing
                               Just (turn, piece) -> Just (PlacedPiece turn piece)
    startingPositions = Map.fromList [
      ((1,2), (0, Queen)),
      ((2,8), (0, King)),
      ((5,5), (0, Rook)),
      ((7,1), (0, Knight)),
      ((8,7), (0, Bishop))]

rowChecksum7 = map makeLegalDecompositions [7, 1890, 8, 10080, 20, 840, 144, 1260]
colChecksum7 = map makeLegalDecompositions [2744, 36, 375, 336, 108, 240, 20, 504]
checksums7 = Checksums rowChecksum7 colChecksum7

-- main = print $ run startingBoard checksums7
main = mapM_ print $ run startingBoard checksums7
