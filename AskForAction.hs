-- TODO document
module AskForAction where

import HaskellMinesweeper

import Text.Read (readMaybe)

askforaction :: State -> Int -> IO Action
askforaction state size =
 do
  putStrLn "What move would you like to make?"
  line <- getLine
  case (readMaybe line :: Maybe Action) of
   Nothing ->
    invalidactionmsg state size
   Just action ->
    if invalidrange (getcoords action) size then
     invalidrangemsg state size
    else if invalidflag action state then
     invalidflagmsg state size
    else
     return action

invalidactionmsg :: State -> Int -> IO Action
invalidactionmsg state size =
 do
  putStrLn "Please use the following formats:"
  putStrLn "Flag (x,y)"
  putStrLn "Uncover (x,y)"
  putStrLn ("...where x and y are integers less than " ++ (show size))
  askforaction state size

invalidrange :: Coordinate -> Int -> Bool
invalidrange (x,y) size = (x >= size) && (y >= size)

invalidrangemsg :: State -> Int -> IO Action
invalidrangemsg state size =
 do
  putStrLn "Out of bounds."
  putStrLn ("x and y must be integers less than " ++ (show size))
  askforaction state size

invalidflag :: Action -> State -> Bool
invalidflag (Uncover _) state = False
invalidflag (Flag coord) (State uncovered _ _ _) = elem coord uncovered

invalidflagmsg :: State -> Int -> IO Action
invalidflagmsg state size =
 do
  putStrLn "You cannot flag an uncovered cell."
  askforaction state size

{--
Test cases TODO

--}

