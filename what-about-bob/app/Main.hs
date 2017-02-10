module Main where

type Target = Int
type LastUsed = Maybe Int
type PossibleChoices = [Int]
data Result = Loss | Win deriving (Eq, Ord, Show)

solveState :: Target -> LastUsed -> PossibleChoices -> Result
solveState target lastUsed ns =
  let
    allValues = filter (<= target) ns
    possibleValues = case lastUsed of
      (Just n) -> filter (/= n) allValues
      otherwise -> allValues
  in
    if (target `elem` possibleValues)
      then Win
      else if (all (\n -> n > target) possibleValues)
        then Loss
        else
          if any (== Loss) $ [solveState (target - n) (Just n) ns | n <- possibleValues]
            then Win
            else Loss

main :: IO ()
main = print $ take 3 $ filter (\(n, r) -> r == Loss) $ zip [1..] (map (\n -> solveState n Nothing [1..9]) [1..])
