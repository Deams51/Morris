module Morris where
 
import Test.QuickCheck
import Data.List



data Morris = Morris {rows :: [[Maybe Cell]]}
  deriving ( Show, Eq )

data Cell = PlayerA | PlayerB | Closed 
  deriving (Show, Eq)


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

printMorris :: Morris -> IO ()
printMorris s = putStr (unlines (map (map cellToChar) (rows s)))

-- Translates Cell into printable values
cellToChar :: Maybe Cell -> Char
cellToChar (Just PlayerA) = 'A'
cellToChar (Just PlayerB) = 'B'
cellToChar (Just Closed)  = ' ' 
cellToChar Nothing        = 'O'
-- blankMorris
-- printMorris
-- isEmpty Cell
-- placeUnit Pos Cell
-- possibleMoves Pos
-- inMill :: Cell
-- allMill
