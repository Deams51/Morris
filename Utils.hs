module Utils where

import Data.Maybe (listToMaybe)

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
