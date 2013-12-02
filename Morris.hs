module Morris where
 
import Test.QuickCheck
import Data.List
import Data.Maybe
import Data.Sequence

data Morris = Morris {rows :: Seq( Seq(Maybe Cell)) }
  deriving ( Show, Eq )

--           Phase,PlA,PlB
type Game = (Int, Int, Int)

data Cell = PlayerA | PlayerB | Closed 
  deriving (Show, Eq)

type Pos = (Int,Int)

blankMorris = Morris ( 
     fromList [
      fromList [Nothing, Just Closed, Just Closed, Nothing, Just Closed, Just Closed, Nothing]
    , fromList [Just Closed, Nothing, Just Closed, Nothing, Just Closed, Nothing, Just Closed]
    , fromList [Just Closed, Just Closed, Nothing, Nothing, Nothing, Just Closed, Just Closed]
    , fromList [Nothing, Nothing, Nothing, Just Closed, Nothing, Nothing, Nothing]
    , fromList [Just Closed, Just Closed, Nothing, Nothing, Nothing, Just Closed, Just Closed]
    , fromList [Just Closed, Nothing, Just Closed, Nothing, Just Closed, Nothing, Just Closed]
    , fromList [Nothing, Just Closed, Just Closed, Nothing, Just Closed, Just Closed, Nothing]
    ])

verticalMills :: [[Pos]]
verticalMills = 
  [ 
    [(0,0),(0,3),(0,6)],
    [(1,1),(1,3),(1,5)],
    [(2,2),(2,3),(2,4)],
    [(3,0),(3,1),(3,2)],
    [(3,4),(3,5),(3,6)],
    [(4,2),(4,3),(4,4)],
    [(5,1),(5,3),(5,5)],
    [(6,0),(6,3),(6,6)]
  ]

horizontalMills :: [[Pos]]
horizontalMills = [[(y,x)|(x,y)<-mill] |mill<-v]
  where v = verticalMills
  
possibleMills :: [[Pos]]
possibleMills = verticalMills ++ horizontalMills





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

-- Return value of (x,y) in morris
getValue :: Morris -> Pos -> Maybe Cell
getValue m (x,y) = index (index (rows m) y) x

-- Checks if a piece can be placed in the position
  -- ie not closed and empty
isValidPos :: Morris -> Pos -> Bool
isValidPos m (x,y) = isNothing(value) && value /= (Just Closed)
  where value = getValue m (x,y)

-- Checks if a position contains a piece from a player
isUsed :: Morris -> Cell -> Pos -> Bool
isUsed m pl (x,y) = value == Just pl
  where value = getValue m (x,y)

-- Check if a stone is in a mill
  --in a mill if pos a stone and if in list of mills and a mill
isInMill :: Morris -> Pos -> Bool 
isInMill m (x,y)  | not(isValidPos m (x,y)) = False
                  | otherwise = or[and[getValue m pos == current |pos<-mill] | mill<-listMills]
  where listMills = Data.List.filter (elem (x,y)) possibleMills
        current = getValue m (x,y)

-- Check if all the stones from a player are in a mill
allMill :: Morris -> Cell -> Bool
allMill m p = and[isInMill m x | x<-playerPos]
  where allPos = [(x,y) | x<-[1..7], y<-[1..7]]
        playerPos = Data.List.filter (isUsed m p) allPos

-- Add a piece to the board at newPos
addPiece :: Morris -> Pos -> Cell -> Morris
addPiece m (x,y) pl = Morris(update y new (rows m))
  where new = update x (Just pl) (index (rows m) y)  

-- Move a piece from oldPos to newPos
movePiece :: Morris -> Pos -> Pos -> Morris
movePiece m pOld p = addPiece (removePiece m p) p pl
  where Just pl = getValue m p

-- Remove a piece
removePiece :: Morris -> Pos -> Morris
removePiece m (x,y) = Morris(update y new (rows m))
  where new = update x Nothing (index (rows m) y) 



 
  

--Phase 2 
-- CanMove old_pos new_pos
-- possibleMoves Pos
 