module Tetra where

import Prelude hiding (Left, Right)

data OrientedTetra a = UpTetra (Tetra a) | DownTetra (Tetra a) deriving (Show)

data Tetra a = Tetra {
    middle :: a
  , peak :: a
  , right :: a
  , left :: a
} deriving (Show)

tetraFaceValue :: OrientedTetra a -> a
tetraFaceValue (UpTetra tetra) = middle tetra
tetraFaceValue (DownTetra tetra) = middle tetra

data Move = MoveLeft | MoveRight | MoveOppositePeak
moves = [MoveLeft, MoveRight, MoveOppositePeak]

roll :: Move -> OrientedTetra a -> OrientedTetra a
roll MoveLeft (UpTetra (Tetra m p r l)) =
  DownTetra (Tetra l r m p)
roll MoveRight (UpTetra (Tetra m p r l)) =
  DownTetra (Tetra r l p m)
roll MoveOppositePeak (UpTetra (Tetra m p r l)) =
  DownTetra (Tetra p m r l)
-- can probs express these using symmetry
roll MoveLeft (DownTetra (Tetra m p r l)) =
  UpTetra (Tetra l r m p)
roll MoveRight (DownTetra (Tetra m p r l)) =
  UpTetra (Tetra r l p m)
roll MoveOppositePeak (DownTetra (Tetra m p r l)) =
  UpTetra (Tetra p m r l)

-- tetra = Tetra "m" "p" "r" "l"
-- oTetra = UpTetra tetra
