module Morris where
 
import Test.QuickCheck
import Data.List (filter, nub, maximumBy, minimumBy, intersect)
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
  deriving ( Eq )

instance Show Morris where
  show (Morris a) = printMorrisString (Morris a)

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

debugMorris = Morris ( 
     fromList [
      fromList [Just PlayerA, Just Closed, Just Closed, Just PlayerA, Just Closed, Just Closed, Nothing]
    , fromList [Just Closed, Just PlayerA, Just Closed, Nothing, Just Closed, Nothing, Just Closed]
    , fromList [Just Closed, Just Closed, Nothing, Nothing, Nothing, Just Closed, Just Closed]
    , fromList [Just PlayerA, Nothing, Nothing, Just Closed, Nothing, Nothing, Nothing]
    , fromList [Just Closed, Just Closed, Nothing, Nothing, Nothing, Just Closed, Just Closed]
    , fromList [Just Closed, Nothing, Just Closed, Nothing, Just Closed, Nothing, Just Closed]
    , fromList [Just PlayerB, Just Closed, Just Closed, Nothing, Just Closed, Just Closed, Just PlayerB]
    ])

newGame :: Game
newGame = (blankMorris,0, PlayerA)

p2Morris = Morris ( 
     fromList [
      fromList [Just PlayerA, Just Closed, Just Closed, Nothing, Just Closed, Just Closed, Just PlayerA]
    , fromList [Just Closed, Just PlayerA, Just Closed, Nothing, Just Closed, Nothing, Just Closed]
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

printMorris :: Morris -> IO ()
printMorris s = putStr (unlines (map (map cellToChar) list))
  where list = [toList x | x<-toList(rows s)]

printMorrisString :: Morris -> String
printMorrisString s = "\n" ++ (unlines (map (map cellToChar) list))
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
  where allPos = [(x,y) | x<-[0..6], y<-[0..6]]
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
    putStrLn ""
    putStrLn ("Phase 1 - Turn : " ++ show s)
    (m,s,c) <- turnP1 (m,s,c)
    playAI (m,s+1,opponent c)

millCreatedAI :: Game -> Pos -> Pos -> IO Game
millCreatedAI (m,s,c) pos del = do
    if(isInMill m pos)
      then do
        mT <- evaluate (removePiece m del)
        return (mT,s,c)
      else do
        return (m,s,c)

turnP1AI :: Game -> IO Game
turnP1AI (m,s,c) = do
    putStrLn ((show c) ++ " turn")
    let ((x,y),(x2,y2)) = bestMoveP1 (m,(-1,-1),(-1,-1),c) 2   
    putStrLn ("AI choose : " ++ show (y,x))
    mT <- evaluate (addPiece m (y,x) c)
    g <- millCreatedAI (mT,s,c) (y,x) (y2,x2) 
    return g

nextTurnP1AI :: Game ->  IO Game
nextTurnP1AI (m,s,c) = do 
    printMorris m
    putStrLn ""
    putStrLn ("Phase 1 - Turn : " ++ show s)
    (m,s,c) <- turnP1AI (m,s,c)
    play (m,s+1,opponent c)

--Phase 2 

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
    putStrLn $ show [ (x1,x) | ((x,x1),y)<-possibleMoves m (Just c)]
    x<- promptIntFromRange "Stone x" (0,6)
    y<- promptIntFromRange "Stone y" (0,6)
    if (isPlayer m (x,y) (Just c))
      then do
        putStrLn $ show $ [(x2,x1) | (x1,x2)<-filter (\(a,b) -> a == (x,y)) $ possibleMoves m (Just c)]
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

playAI :: Game -> IO Game
playAI g  | isPhase1 g  = nextTurnP1AI g >>= play
          | isDone g    = return g
          | otherwise   = nextTurnP2 g >>= play

-- Check if a game is done
isDone :: Game -> Bool
isDone (m,t,c) = numberStones m PlayerA <4
              || numberStones m PlayerB <4


--AI
-- Int : Move that get us here
-- Int : Stone removed to get us here
-- Cell : Player who played the move
type StateP1 = (Morris,Pos,Pos,Cell)

data Tree = Empty | Node StateP1  [Tree] deriving (Show) 

consTreeP1 :: StateP1 -> Int -> Tree 
consTreeP1 t 0              = (Node t [])  
consTreeP1 (m,pos,del,p) n  = (Node (m,pos,del,p) [consTreeP1 state (n-1) | state<-newStates])
  where newStates = concat[addPieceAI m x p|x<-possiblePlaces m]

addPieceAI :: Morris -> Pos -> Cell -> [StateP1]
addPieceAI m x p  | isInMill newM x = [(removePiece newM delete,x,delete,opponent p) | delete<-canBeRemoved newM (opponent p)]
                  | otherwise =  [(newM,x,(-1,-1),opponent p)]
  where newM = addPiece m x p


minimax :: Tree -> Int -> Bool -> Int
minimax (Node (m,pos,del,p) l) 0 b = heuristic m
minimax (Node (m,pos,del,p) l) n b  | b = maximum [minimax child (n-1) False | child<-l]
                                    | otherwise = minimum [minimax child (n-1) True  | child<-l]

bestMoveP1 :: StateP1 -> Int -> (Pos,Pos)
bestMoveP1 s n = (y,z)
  where (Node node l) = consTreeP1 s n
        order (a,b,c) (d,e,f) = compare a d
        (x,y,z) = maximumBy order [(minimax (Node (m,pos,del,p) next) (n-1) False,pos,del) | (Node (m,pos,del,p) next)<-l]

heuristic :: Morris -> Int
heuristic m = 10*(countMils m PlayerB) - 9*(countMils m PlayerA)

-- gives the current value of the Morris a rating 
rateMorris :: Morris -> Cell -> Int
rateMorris m c = f c - f (opponent c) + 9*(countMils m c) - 12*(countMils m (opponent c))
  where list = concat [toList x | x<-toList(rows m)]
        f x  = length.filter (==(Just x)) $ list

-- returns all possible moves of a player
possibleMoves :: Morris -> Maybe Cell -> [(Pos,[Pos])]
possibleMoves m c = [cellMoves m x c | x <-myCells m c]

myCells :: Morris -> Maybe Cell -> [Pos]
myCells m c = filter (\x -> isPlayer m x c) (nub $ concat possibleMills)

-- returns a list of all the player stones that can be removed if we just created a mill
canBeRemoved :: Morris -> Cell -> [Pos]
canBeRemoved m p  | allMill m p = myCells m (Just p)
                  | otherwise = [x | x<-myCells m (Just p), not $ isInMill m x]

-- not returning a correct result, blame isValidMove
-- cellMoves p2Morris (1,1) (Just PlayerA)

-- returns the possible moves of a cell
cellMoves :: Morris -> Pos -> Maybe Cell -> (Pos,[Pos])
cellMoves m p c = (p, filter (\x -> isValidMove m p x c) allCord)
  where allCord = [(x,y) | x<-[0..6], y<-[0..6]]

-- returns coordinates of all places on the table not occupied by a stone
possiblePlaces :: Morris -> [Pos]        
possiblePlaces m = map (\(x,y) -> x) 
                   $ filter (\(x,y) -> y == Nothing) (morrisCoords m)

-- returns a morris as a flat list with coordintes
morrisCoords :: Morris -> [(Pos, Maybe Cell)]
morrisCoords m = zip [(x,y) | x<-[0..6], y<-[0..6]] 
                $ concat [toList x | x<-toList(rows m)] 

-- counts the mils of a player
countMils :: Morris -> Cell -> Int
countMils m c = length $ filter (>2) 
                [length $ intersect x (myStonePos m c) | x<-possibleMills]

-- Returns the position of all stones owned by player
myStonePos :: Morris -> Cell -> [Pos]
myStonePos m c = map (\(x,y) -> x) $ filter (\(x,y) -> y == (Just c)) $ morrisCoords m

-- possiblePlaces ... place all
  -- check if mil
      -- remove enemy piece if mill
-- repeat for enemy
--phaseOneAI :: Morris ->
--phaseOneAI m c d = Node m [x | x<-ps]
--  where ps = possiblePlaces m

--evaluateMorris :: B -> (A,[B])
--evaluateMorris b = 

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
