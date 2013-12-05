module Morris where
 
import Test.QuickCheck
import Data.List
import Data.Maybe
import Data.Sequence
import Data.Foldable (toList)

data Cell = PlayerA | PlayerB | Closed 
  deriving (Show, Eq)

type Pos = (Int,Int)

data Morris = Morris {rows :: Seq( Seq(Maybe Cell)) }
  deriving ( Show, Eq )

--           Phase,PlA,PlB
--type State = (Int, Int, Int)
-- A Game is a Morris board and a number of turns played
type Game = (Morris, Int)

type TurnP1 = (Int, Int)



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

newGame :: Game
newGame = (blankMorris,0)

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
horizontalMills = [[(y,x)|(x,y)<-mill] |mill<-verticalMills]
  
possibleMills :: [[Pos]]
possibleMills = verticalMills ++ horizontalMills


--blankMorris :: Morris
--blankMorris = Morris [[Nothing | x<-[0..6]] | x<-[0..6]]

printMorris :: Morris -> IO ()
printMorris s = putStr (unlines (map (map cellToChar) list))
  where list = [toList x | x<-toList(rows s)]

-- Translates Cell into printable values
cellToChar :: Maybe Cell -> Char
cellToChar (Just PlayerA) = 'A'
cellToChar (Just PlayerB) = 'B'
cellToChar (Just Closed)  = ' ' 
cellToChar Nothing        = 'O'

-- Phase 1:
-- Adding_a_piece morris player (a,b)
--  only add the piece from player to (a,b) if (a,b) is Nothing

-- Return value of (x,y) Cell in morris
getValue :: Morris -> Pos -> Maybe Cell
getValue m (x,y) = index (index (rows m) y) x

-- Checks if a piece can be placed in the position
  -- ie not closed and empty
isValidPos :: Morris -> Pos -> Bool
isValidPos m (x,y) = isNothing(value) && value /= (Just Closed)
  where value = getValue m (x,y)

-- Checks if a move from posF to posT is valid
  -- For a move to be valid from posF, posT must be in a mill with PosF 
isValidMove :: Morris -> Pos -> Pos -> Bool
isValidMove = undefined

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

nextTurnP1 :: Game ->  IO Game
nextTurnP1 (m,s) = do 
    printMorris m
    posA <- inputTurnP1 (m,s)
    printMorris (addPiece m posA PlayerA)
    posB <- inputTurnP1 (addPiece m posA PlayerA,s+1)
    play $ (addPiece (addPiece m posA PlayerA) posB PlayerB,s+1)

nextTurnP2 :: Game -> IO Game
nextTurnP2 g = return g

-- Check if a game is done
  -- ie : State superior to 1 and one player with less than 4 stones
isDone :: Game -> Bool
isDone (m,t) = False

-- Check if still in Phase 1
isPhase1 :: Game -> Bool
isPhase1 (m,t) = t<18 
  

play :: Game -> IO Game
play g  | isDone g    = return g
        | isPhase1 g  = nextTurnP1 g >>= play
        | otherwise   = nextTurnP2 g >>= play






inputTurnP1 :: Game -> IO TurnP1
inputTurnP1 (m,s) = do 
    x<- promptIntFromRange "x" (0,6)
    y<- promptIntFromRange "y" (0,6)
    if (isValidPos m (x,y))
      then return (x, y)
      else do
        putStrLn "Invalid position, try again." 
        (x,y) <- inputTurnP1 (m,s)
        return (x,y)


promptIntFromRange :: String -> (Int, Int) -> IO Int
promptIntFromRange msg (from, to) = promptInt newMsg inRange 
  where newMsg = concat [msg, "[", show from, ";", show to, "]"]
        inRange v = v >= from && v <= to

promptInt :: String -> (Int -> Bool) -> IO Int
promptInt msg p = do 
    putStr (msg ++ "> ")
    mx <- maybeReadLn
    case mx of
        Just x | p x -> return x
        _            -> promptInt msg p

maybeReadLn :: (Read a) => IO (Maybe a)
maybeReadLn = fmap maybeRead getLine

maybeRead :: (Read a) => String -> Maybe a
maybeRead str = listToMaybe [x | (x, "") <- reads str]
--Phase 2 
-- CanMove old_pos new_pos
-- possibleMoves Pos
{-
 instance Arbitrary Sudoku where
 arbitrary =
   do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
      return (Sudoku rows)

sequence :: Monad m => [m a] -> m [a]


-}
cell :: Int -> Int -> Gen (Maybe Cell)
cell a b = frequency
       [
         (1, return Nothing)
       , (a, return (Just PlayerA))
       , (b, return (Just PlayerB))                
       ]
