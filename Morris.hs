module Morris where
 
import Test.QuickCheck
import Data.List
import Data.Maybe
import Data.Sequence

data Morris = Morris {rows :: [[Maybe Cell]]}
  deriving ( Show, Eq )

--           Phase,PlA,PlB
type Game = (Int, Int, Int)

data Player = PlayerA | PlayerB
data Cell = Player | Closed 
  deriving (Show, Eq)

type Pos = (Int,Int)

blankMorris = Morris 
    [
      [Nothing, Just Closed, Just Closed, Nothing, Just Closed, Just Closed, Nothing]
    , [Just Closed, Nothing, Just Closed, Nothing, Just Closed, Nothing, Just Closed]
    , [Just Closed, Just Closed, Nothing, Nothing, Nothing, Just Closed, Just Closed]
    , [Nothing, Nothing, Nothing, Just Closed, Nothing, Nothing, Nothing]
    , [Just Closed, Just Closed, Nothing, Nothing, Nothing, Just Closed, Just Closed]
    , [Just Closed, Nothing, Just Closed, Nothing, Just Closed, Nothing, Just Closed]
    , [Nothing, Just Closed, Just Closed, Nothing, Just Closed, Just Closed, Nothing]
    ]

--blankMorris :: Morris
--blankMorris = Morris [[Nothing | x<-[0..6]] | x<-[0..6]]

--printMorris :: Morris -> IO ()
--printMorris s = putStr (unlines (map (map cellToChar) (rows s)))

-- Translates Cell into printable values
--cellToChar :: Maybe Cell -> Char
--cellToChar (Just PlayerA) = 'A'
--cellToChar (Just PlayerB) = 'B'
--cellToChar (Just Closed)  = ' ' 
--cellToChar Nothing        = 'O'

-- Phase 1:
-- Adding_a_piece morris player (a,b)
--  only add the piece from player to (a,b) if (a,b) is Nothing

-- Checks if a piece can be placed in the position
isEmpty :: Morris -> Pos -> Bool
isEmpty m (x,y) = isNothing((rows m!!y)!!x)

-- 
addPiece :: Morris -> Pos -> Pos -> Player -> Bool
addPiece m (xOld,yOld) (xNew,yNew) pl = undefined

-- Check if current cell is in a mill
isInMill :: Morris -> Pos -> Bool 
isInMill m p = undefined

allMill :: Morris -> Player -> Bool
allMill m p = undefined

-- Tries to remove a piece
removePiece :: Morris -> Pos -> Bool
removePiece m p = undefined  

 
  

--Phase 2 
-- CanMove old_pos new_pos
-- possibleMoves Pos
 