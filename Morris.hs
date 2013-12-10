module Morris where
 
import Test.QuickCheck
import Data.List (filter, nub)
import Data.Maybe (isNothing)
import Data.Sequence (Seq, fromList, index, update)
import Data.Foldable (toList)
import Utils
import Control.Monad (when)
import Control.Exception.Base (evaluate)
import Data.Maybe (fromJust)


data Cell = PlayerA | PlayerB | Closed 
  deriving (Show, Eq)

type Pos = (Int,Int)
type Move = (Pos,Pos)


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
p2Game = (p2Morris,18, PlayerA)

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
addPiece m (x,y) pl | not $ isValidPos m (x,y) = m
                    | otherwise = Morris(update y new (rows m))
  where new = update x (Just pl) (index (rows m) y)  

-- Move a piece from oldPos to newPos
movePiece :: Morris -> Pos -> Pos -> Morris
movePiece m pOld p = removePiece (addPiece m p pl) pOld
  where Just pl = getValue m pOld

-- Remove a piece
removePiece :: Morris -> Pos -> Morris
removePiece m (x,y) |  val == Just Closed  || isNothing val = m
                    | otherwise = Morris(update y new (rows m))
  where new = update x Nothing (index (rows m) y) 
        val = getValue m (x,y)
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
                          && not (isNothing p)
                          && isAdjacent posF posT

isAdjacent :: Pos -> Pos -> Bool
isAdjacent (x,y) (xT,yT) = or[elem (x,y) mills && elem (xT,yT) mills | mills<-possibleMills]
                          && and[all (>=distFT) (map (distanceSquare (x,y)) (filter (/=(x,y)) mills))| mills<-possibleMills, elem (x,y) mills, elem (xT,yT) mills]
  where distFT = distanceSquare (x,y) (xT,yT)

distanceSquare :: Pos -> Pos -> Int
distanceSquare (x,y) (x2,y2) = (x-x2)*(x-x2) + (y-y2)*(y-y2)

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


---
 

--no nrecursive prototype, fix

--emulate :: Morris -> Maybe Cell -> Pos
--emulate m c  =  [(fst x
--                 ,[(rateMorris (movePiece m (fst x) z) (fromJust c), z) 
 --                 | z<-(snd x)]) 
   --             | x<-(possibleMoves p2Morris (Just PlayerB))]
 -- where x    = [x | x<-(possibleMoves m c)]
        

-- emulate' m c d =  [emulate' (movePiece m (fst x) (snd x)) c (d-1) | x<-possibleMoves m c]
-- emulate' m (Just c) 0 = [rateMorris m c]
   
-- creates a game tree of maximum depth d   
--makeTree m c d = (Nothing, makeTree' m c d)

--makeTree' m c d = [ | x<-(possibleMoves m c)]



data Tree a = EmptyTree | Node a [Tree a]
  deriving (Show, Read, Eq)  


-- makeTree m c d = Node m [makeTree' m c d x y | (x,y)<-possibleMoves m c]

-- makeTree' m c d x y = Node m [makeTree' (movePiece m a b) c (d-1) a b | (a,b)<-possibleMoves m c]

-- gives the current value of the Morris a rating 
rateMorris :: Morris -> Cell -> Int
rateMorris m c = f c - f (opponent c)
  where list = concat [toList x | x<-toList(rows m)]
        f x  = length.filter (==(Just x)) $ list

-- returns all possible moves of a player
possibleMoves :: Morris -> Maybe Cell -> [(Pos,[Pos])]
possibleMoves m c = [cellMoves m x c | x <-myCells]
  where myCells = filter (\x -> isPlayer m x c) (nub $ concat possibleMills)

-- not returning a correct result, blame isValidMove
-- cellMoves p2Morris (1,1) (Just PlayerA)

-- returns the possible moves of a cell
cellMoves :: Morris -> Pos -> Maybe Cell -> (Pos,[Pos])
cellMoves m p c = (p, filter (\x -> isValidMove m p x c) allCord)
  where allCord = [(x,y) | x<-[0..6], y<-[0..6]]

-- returns coordinates of all places on the table not occupied by a stone
possiblePlaces :: Morris -> [Pos]        
possiblePlaces m = map (\(x,y) -> x) $ filter (\(x,y) -> y == Nothing) 
                   $ zip [(x,y) | x<-[0..7], y<-[0..7]] 
                   $ concat [toList x | x<-toList(rows m)]


-- Properties related func 
cell :: Gen (Cell)
cell = frequency
       [
        (1, return PlayerA),
        (1, return PlayerB)                
       ]

instance Arbitrary Morris where
  arbitrary = 
    do
      return blankMorris

instance Arbitrary Cell where
  arbitrary = 
    do
      c<- cell
      return c


prop_addPiece :: Morris -> Pos -> Cell -> Bool
prop_addPiece m (x,y) c | not $ isValidPos m (x',y') = m == mN 
                        | otherwise = getValue mN (x',y') == Just c
  where x' = abs $ mod x 7
        y' = abs $ mod y 7
        mN = addPiece m (x',y') c

prop_removePiece :: Morris -> Pos -> Bool
prop_removePiece m (x,y)  | (val == Just Closed) = mN == m
                          | otherwise = getValue mN (x',y') == Nothing 
  where x'  = abs $ mod x 7
        y'  = abs $ mod y 7
        mN  = removePiece m (x',y')
        val = getValue m (x',y')

-- Need to check that the new morris have the piece at the right position
  -- if the move is not valid then the morris remains the same
{- prop_movePiece :: Morris -> Pos -> Pos -> Cell -> Bool
prop_movePiece m (x,y) (xT,yT) c  | isValidMove m (x',y') (xT',yT') (Just c) = getValue m (x',y') == getValue m (xT',yT')
                                  | otherwise = m == mN
  where x'  = abs $ mod x 7
        y'  = abs $ mod y 7
        xT' = abs $ mod xT 7
        yT' = abs $ mod yT 7
        mN  = movePiece m (x',y') (xT',yT')
-}
