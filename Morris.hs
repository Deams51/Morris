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
    [(1,1),(1,4),(1,7)],
    [(2,2),(2,4),(2,6)],
    [(3,3),(3,4),(3,5)],
    [(4,1),(4,2),(4,3)],
    [(4,5),(4,6),(4,7)],
    [(5,3),(5,4),(5,5)],
    [(6,2),(6,4),(6,6)],
    [(7,1),(7,4),(7,7)]
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

-- Add a piece to the board at newPos
addPiece :: Morris -> Pos -> Cell -> Morris
addPiece m (x,y) pl = Morris(update y new (rows m))
  where new = update x (Just pl) (index (rows m) y)  

-- Move a piece from oldPos to newPos
movePiece :: Morris -> Pos -> Pos -> Morris
movePiece m (xOld,yOld) (x,y) = undefined 

-- Check if a stone is in a mill
  --in a mill if pos a stone and if in list of mills and a mill
isInMill :: Morris -> Pos -> Bool 
isInMill m (x,y)  | not(isValidPos m (x,y)) = False
                  | otherwise = or[and[getValue m pos == current |pos<-mill] | mill<-listMills]
  where listMills = Data.List.filter (elem (x,y)) possibleMills
        current = getValue m (x,y)

allMill :: Morris -> Cell -> Bool
allMill m p = undefined

-- Tries to remove a piece
removePiece :: Morris -> Pos -> Bool
removePiece m p = undefined  

 
  

--Phase 2 
-- CanMove old_pos new_pos
-- possibleMoves Pos
 