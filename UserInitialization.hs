module UserInitialization where

import HaskellMinesweeper
import Mines
import Text.Read (readMaybe)


-- User is prompted to enter an Int greater than 0 to set grid size
-- Repeats if given an invalid response
userGridSize :: IO Int
userGridSize = do
       putStrLn "Enter single integer (> 1) for grid length and width"
       line <- getLine
       case (readMaybe line :: Maybe Int) of 
           Nothing ->
                userGridSize
           Just size ->
                if size <= 1 then
                    userGridSize
                else
                    return size


-- Prompts user to enter an integer to dtermine the numer of mines they would like to have
-- Repeats if given an invalid response
userMineCount :: Int -> IO Int
userMineCount board = do
       putStrLn ("Enter number of mines greater than 0 and less than "  ++  show (board*board))
       line <- getLine
       case (readMaybe line :: Maybe Int) of 
           Nothing ->
                userMineCount board
           Just mines ->
                if invalidMineCount mines board  then
                    userMineCount board
                else
                    return mines


-- Returns true if given an invalid number of mines
-- invalid mines are negative or greater than the number of squares
invalidMineCount :: Int -> Int -> Bool
invalidMineCount mines gridSize = (mines <= 0) || (mines >= gridSize*gridSize)


             



 



