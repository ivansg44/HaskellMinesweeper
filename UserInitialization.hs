module UserInitialization where

import HaskellMinesweeper
import Mines
import Text.Read (readMaybe)

userGridSize :: IO Int
userGridSize = do
       putStrLn "Enter single positive integer (>0) for grid length and width"
       line <- getLine
       case (readMaybe line :: Maybe Int) of 
           Nothing ->
                userGridSize
           Just size ->
                if zeroOrNegGrid size then
                    userGridSize
                else
                    return size


zeroOrNegGrid :: Int -> Bool
zeroOrNegGrid num = num <= 0


userMineCount :: Int -> IO Int
userMineCount board = do
       putStrLn ("Enter number of mines greater than 0 and  less than "  ++  show (board*board))
       line <- getLine
       case (readMaybe line :: Maybe Int) of 
           Nothing ->
                userMineCount board
           Just mines ->
                if invalidMineCount mines board  then
                    userMineCount board
                else
                    return mines

invalidMineCount :: Int -> Int -> Bool
invalidMineCount mines gridSize = (mines < 0) || (mines > gridSize*gridSize)


             



 



