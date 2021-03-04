-- This module handles user input.
module AskForAction where

import HaskellMinesweeper

import Text.Read (readMaybe)

-- Entry point function that asks users for input, and eventually returns
-- action.
askforaction :: State     -- Current state of game
             -> Int       -- Size of board
             -> IO Action -- Returns action or branches to functions for error
                          -- handling.
askforaction state size =
 do
  putStrLn "What move would you like to make?"
  line <- getLine
  case (readMaybe line :: Maybe Action) of
   Nothing ->
    -- Invalid action
    invalidactionmsg state size
   Just action ->
    -- Invalid range of coordinates in action
    if invalidrange (getcoords action) size then
     invalidrangemsg state size
    -- Tried to flag uncovered cell
    else if invalidflag action state then
     invalidflagmsg state size
    else
     -- All good
     return action

-- Tells user they have inputted an invalid action
invalidactionmsg :: State     -- Current state of game
                 -> Int       -- Size of board
                 -> IO Action -- Branches back to askforaction
invalidactionmsg state size =
 do
  putStrLn "Please use the following formats:"
  putStrLn "Flag (row,column)"
  putStrLn "Uncover (row,column)"
  putStrLn ("...where row and column are integers less than " ++ (show size))
  askforaction state size

-- Checks if a coordinate is invalid given size of board
invalidrange :: Coordinate -- Coordinate to check
             -> Int        -- Size of board
             -> Bool       -- Returns true if invalid coordinate
invalidrange (x,y) size = (x >= size) || (y >= size) || (x < 0) || (y < 0)

-- Tells user they have inputted an action with an invalid range
invalidrangemsg :: State     -- Current state of game
                -> Int       -- Size of board
                -> IO Action -- Branches back to askforaction
invalidrangemsg state size =
 do
  putStrLn "Out of bounds."
  putStrLn ("row and column must be integers less than " ++ (show size))
  askforaction state size

-- Checks if user tried to flag uncovered cell
invalidflag :: Action -- Action by user
            -> State  -- Current state of game
            -> Bool   -- Returns true if user tried to flag uncovered cell
invalidflag (Flag coord) (State uncovered _ _ _) = elem coord uncovered
-- User did not flag a cell
invalidflag _ _ = False

-- Tells user they have tried to flag an uncovered cell
invalidflagmsg :: State     -- Current state of game
               -> Int       -- Size of board
               -> IO Action -- Branches back to askforaction
invalidflagmsg state size =
 do
  putStrLn "You cannot flag an uncovered cell."
  askforaction state size

