module Morris where
 
import Test.QuickCheck
import Data.List (filter)
import Data.Maybe (isNothing)
import Data.Sequence (Seq, fromList, index, update)
import Data.Foldable (toList)
import Utils
import Control.Monad (when)
import Control.Exception.Base (evaluate)

data Cell = PlayerA | PlayerB | Closed 
  deriving (Show, Eq)

type Pos = (Int,Int)

data Morris = Morris {rows :: Seq( Seq(Maybe Cell)) }
  deriving ( Show, Eq )

type Game = (Morris, Int, Cell)

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
newGame = (blankMorris,0, PlayerA)

-- Example of phase 2 Morris to debug
{-p2Morris = Morris ( 
     fromList [
      fromList [Just PlayerA, Just Closed, Just Closed, Nothing, Just Closed, Just Closed, Just PlayerA]
    , fromList [Just Closed, Just PlayerA, Just Closed, Just PlayerA, Just Closed, Nothing, Just Closed]
    , fromList [Just Closed, Just Closed, Just PlayerB, Just PlayerB, Nothing, Just Closed, Just Closed]
    , fromList [Just PlayerB, Just PlayerA, Nothing, Just Closed, Just PlayerA, Just PlayerB, Just PlayerB]
    , fromList [Just Closed, Just Closed, Just PlayerB, Just PlayerA, Nothing, Just Closed, Just Closed]
    , fromList [Just Closed, Nothing, Just Closed, Just PlayerA, Just Closed, Just PlayerB, Just Closed]
    , fromList [Just PlayerA, Just Closed, Just Closed, Just PlayerB, Just Closed, Just Closed, Just PlayerB]
    ])
-}

p2Morris = Morris ( 
     fromList [
      fromList [Just PlayerA, Just Closed, Just Closed, Nothing, Just Closed, Just Closed, Just PlayerA]
    , fromList [Just Closed, Just PlayerA, Just Closed, Just PlayerA, Just Closed, Nothing, Just Closed]
    , fromList [Just Closed, Just Closed, Just PlayerB, Just PlayerB, Nothing, Just Closed, Just Closed]
    , fromList [Nothing, Just PlayerA, Nothing, Just Closed, Just PlayerA, Just PlayerB, Nothing]
    , fromList [Just Closed, Just Closed, Just PlayerB, Just PlayerA, Nothing, Just Closed, Just Closed]
    , fromList [Just Closed, Nothing, Just Closed, Just PlayerA, Just Closed, Nothing, Just Closed]
    , fromList [Just PlayerA, Just Closed, Just Closed, Nothing, Just Closed, Just Closed, Nothing]
    ])

p2Game :: Game
p2Game = (p2Morris,8, PlayerA)

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
cellToChar (Just Closed)  = '.' 
cellToChar Nothing        = 'O'

opponent :: Cell -> Cell
opponent c | c == PlayerA = PlayerB
           | c == PlayerB = PlayerA
-- Phase 1

-- Return value of (x,y) Cell in morris
getValue :: Morris -> Pos -> Maybe Cell
getValue m (x,y) = index (index (rows m) y) x

-- Checks if a piece can be placed in the position
  -- ie not closed and empty
isValidPos :: Morris -> Pos -> Bool
isValidPos m (x,y) = isNothing(value) && value /= (Just Closed)
  where value = getValue m (x,y)

isRemovable :: Morris -> Pos -> Cell -> Bool
isRemovable m pos c = and[val /= x | x<-[Nothing, Just Closed, Just c]]
                        && ((not $ isInMill m pos ) || (allMill m $ opponent c))
    where val = getValue m pos

-- Checks if a position contains a piece from a player
isUsed :: Morris -> Cell -> Pos -> Bool
isUsed m pl (x,y) = value == Just pl
  where value = getValue m (x,y)

-- Check if a stone is in a mill
  --in a mill if pos a stone and if in list of mills and a mill
isInMill :: Morris -> Pos -> Bool 
isInMill m (x,y)  = or[and[getValue m pos == current |pos<-mill] | mill<-listMills]
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
movePiece m pOld p = removePiece (addPiece m p pl) pOld
  where Just pl = getValue m pOld

-- Remove a piece
removePiece :: Morris -> Pos -> Morris
removePiece m (x,y) = Morris(update y new (rows m))
  where new = update x Nothing (index (rows m) y) 

-- Check if still in Phase 1
isPhase1 :: Game -> Bool
isPhase1 (m,t,c) = t<18

millCreated :: Game -> Pos -> IO Game
millCreated (m,s,c) pos = do
    if(isInMill m pos)
      then do
        putStrLn ((show c) ++ " created a mill, chose a stone to remove.")
        x<- promptIntFromRange "x" (0,6)
        y<- promptIntFromRange "y" (0,6)
        if (isRemovable m (x,y) c)
          then do
            mT <- evaluate (removePiece m (x,y))
            when (isInMill mT (x,y)) $ putStrLn "Mill detected"
            return (mT,s,c)
          else do
            putStrLn "Invalid choice, try again." 
            g <- (millCreated (m,s,c) pos)
            return g
      else do
        return (m,s,c)

turnP1 :: Game -> IO Game
turnP1 (m,s,c) = do
    printMorris m
    putStrLn ((show c) ++ " turn")
    x<- promptIntFromRange "x" (0,6)
    y<- promptIntFromRange "y" (0,6)
    if (isValidPos m (x,y))
      then do
        mT <- evaluate (addPiece m (x,y) c)
        g <- millCreated (mT,s,c) (x,y) 
        return g
      else do
        putStrLn "Invalid position, try again." 
        g <- turnP1 (m,s,c)
        return g

nextTurnP1 :: Game ->  IO Game
nextTurnP1 (m,s,c) = do 
    printMorris m
    putStrLn ("Phase 1 - Turn : " ++ show s)
    (m,s,c) <- turnP1 (m,s,c)
    play (m,s+1,opponent c)

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

-- Checks if a stone belongs to a player
isPlayer :: Morris -> Pos -> Maybe Cell -> Bool
isPlayer m pos p = getValue m pos == p

-- Checks if a move from posF to posT is valid
  -- For a move to be valid from posF, posT must be in a possible mill with PosF 
isValidMove :: Morris -> Pos -> Pos -> Maybe Cell -> Bool
isValidMove m posF posT p = (isValidPos m posT)
                          && isPlayer m posF p
                          && or[elem posF mills && elem posT mills | mills<-possibleMills]

numberStones :: Morris -> Cell -> Int
numberStones m c = (length $ filter (== Just c) l )
  where l = concat [toList x | x<-toList(rows m)]

turnP2 :: Game -> IO Game
turnP2 (m,s,c) = do
    printMorris m
    putStrLn ((show c) ++ " turn")
    x<- promptIntFromRange "Stone x" (0,6)
    y<- promptIntFromRange "Stone y" (0,6)
    if (isPlayer m (x,y) (Just c))
      then do
        xT<- promptIntFromRange "To x" (0,6)
        yT<- promptIntFromRange "To y" (0,6)
        if (isValidMove m (x,y) (xT,yT) (Just c))
          then do 
            mT <- evaluate (movePiece m (x,y) (xT,yT))
            g <- millCreated (mT,s,c) (xT,yT)
            return g
          else do
            putStrLn "Invalid move, please try again." 
            g <- turnP2 (m,s,c)
            return g
      else do
        putStrLn "Invalid stone, please choose one of you own." 
        g <- turnP2 (m,s,c)
        return g

nextTurnP2 :: Game -> IO Game
nextTurnP2 (m,s,c) = do
    printMorris m
    putStrLn ("Phase 2 - Turn : " ++ show s)
    (m,s,c) <- turnP2 (m,s,c)
    play (m,s+1, opponent c)

play :: Game -> IO Game
play g  | isPhase1 g  = nextTurnP1 g >>= play
        | isDone g    = return g
        | otherwise   = nextTurnP2 g >>= play

-- Check if a game is done
isDone :: Game -> Bool
isDone (m,t,c) = numberStones m PlayerA <4
              || numberStones m PlayerB <4

-- Properties related func 

cell :: Int -> Int -> Gen (Maybe Cell)
cell a b = frequency
       [
         (1, return Nothing)
       , (a, return (Just PlayerA))
       , (b, return (Just PlayerB))                
       ]
