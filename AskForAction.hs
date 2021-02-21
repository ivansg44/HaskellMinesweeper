-- TODO document
module AskForAction where

import HaskellMinesweeper

import Text.Read (readMaybe)

askforaction :: Int -> IO Action
askforaction size =
 do
  putStrLn "What move would you like to make?"
  line <- getLine
  case (readMaybe line :: Maybe Action) of
   Nothing ->
    askforactionhelper size
   Just action ->
    if validcoord (getcoords action) size then
     return action
    else
     askforactionhelper size

validcoord :: Coordinate -> Int -> Bool
validcoord (x,y) size = (x < size) && (y < size)

askforactionhelper :: Int -> IO Action
askforactionhelper size =
 do
  putStrLn "Please use the following formats:"
  putStrLn "Flag (x,y)"
  putStrLn "Uncover (x,y)"
  putStrLn ("...where x and y are integers less than " ++ (show size))
  askforaction size

{--
Test cases TODO

--}

