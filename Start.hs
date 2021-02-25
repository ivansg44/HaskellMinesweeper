-- Point of entry into program
-- :l Start
-- start
module Start where

import AskForAction
import HaskellMinesweeper
import PrintBoard
import Mines
import UserInitialization

-- This is where it begins
start :: IO ()
start =
 do
  -- Ask user to specify board size
  size <- userGridSize
  -- Ask user to specify number of mines
  mineCount <- userMineCount size
  -- Get blank board string
  initboard <- return (printinitboard size)
  -- Get initial state of game
  initstate <- initialState size mineCount
  -- Start playing
  play initstate size initboard

-- This is where the game branches out into paths for victory, or continuing.
-- Game comes back here whenever you need to check for victory.
play :: State  -- State of game being played
     -> Int    -- Size of board
     -> [Char] -- String representation of board so far
     -> IO ()  -- Goes to finish or continue
play state size oldboard =
 do
  -- Print out the board so far
  putStrLn oldboard
  -- Ask user to input action
  action <- askforaction state size
  -- Plug inputted action and state into game to get result
  result <- return (haskellminesweeper action state)
  -- If result indicates game is over
  if endgame result then
   finish result state oldboard
  -- If result indicates game is not over
  else
   continue (getcoords action) result size oldboard

-- This function begins the process of ending the game
finish :: Result -- This should be an EndOfGame result
       -> State  -- Final state of the game
       -> [Char] -- String representation of board so far. Missing last move.
       -> IO ()  -- Goes to final print function
finish (EndOfGame win) oldstate oldboard =
 do
  -- EndOfGame True means victory
  if win then
   printwinnerboard oldstate oldboard
  -- EndOfGame False means loss
  else
   printloserboard oldstate oldboard

-- Print out winner board
printwinnerboard :: State  -- Final state of the game
                 -> [Char] -- String representation of board so far. Missing
                           -- last uncovered cell.
                 -> IO ()  -- Ends program
printwinnerboard (State _ [last] _ mines) oldboard =
 do
  -- Get the value of the last uncovered cell. Need to do this because
  -- EndOfGame doesn't update state.
  lastval <- return (countmines last mines)
  -- Replace last uncovered coordinate in string representation of board
  new <- return (printnewboard last (head (show lastval)) oldboard)
  -- Print winner's board
  putStrLn new
  putStrLn "You win."

-- Print out loser board
printloserboard :: State  -- Final state of the game
                -> [Char] -- String representation of board so far. Missing
                          -- mine reveal.
                -> IO ()  -- Ends program
printloserboard (State _ _ _ mines) oldboard =
 do
  -- Reveal all mine locations in string representation of board
  new <- return (foldr (\ e acc -> printnewboard e '#' acc) oldboard mines)
  -- Print loser's board
  putStrLn new
  putStrLn "You lose."

-- This function continues the game after the user selects an action that did
-- not result in victory or loss. It will eventually go back to play, but may
-- also branch to continueafterzero, which automatically clears adjacent 0
-- cells if the user cleared a zero cell.
continue :: Coordinate -- Coordinate associated with user action in play
         -> Result     -- Result aassociated with user action in play
         -> Int        -- Size of board
         -> [Char]     -- String representation of board. Missing last move.
         -> IO ()      -- Will go back to play, or continueafterzero.
-- User uncovered a cell
continue coord (ContinueAfterClear val (State u c f m)) size oldboard =
 -- User uncovered a 0 cell.
 if val == 0 then
  continueafterzero [e | e <- todo, not (elem e u)]    -- Covered 0 cells
                                                       -- adjacent to user-
                                                       -- selected cell.
                    (State u c f m)
                    size
                    (printnewboard coord '0' oldboard) -- Update board string
                                                       -- with latest 0.
 -- User uncovered a non-0 cell.
 else
  -- Go back to play with update board string
  play (State u c f m) size (printnewboard coord (head (show val)) oldboard)
 where todo = (getadjacentcoords coord size)
-- User flagged a cell
continue coord
         (ContinueAfterFlag (State uncovered covered flagged mines))
         size
         oldboard =
 -- User flagged non-flagged cell (hence why the game put coord in flagged).
 if elem coord flagged then
  play (State uncovered covered flagged mines)
       size
       -- Update board with flag character
       (printnewboard coord '!' oldboard)
 -- User flagged already-flagged cell (hence why coord not in flagged; game
 -- took it out).
 else
  play (State uncovered covered flagged mines)
       size
       -- Update board back to blank character
       (printnewboard coord '-' oldboard)

-- This is a bit of a complicated function. After the user selects a 0 cell,
-- the path eventually leads here. All adjacent 0 cells are cleared. Any
-- adjacent 0 cells to those cleared cells are also cleared. And so on.
-- Eventually goes back to play, when out of adjacent 0 cells to clear.
continueafterzero :: [Coordinate] -- todo, cells adjacent to 0 cells cleared so
                                  -- far. May or may not be 0.
                  -> State        -- State of game so far
                  -> Int          -- Size of board
                  -> [Char]       -- String representation of board
                  -> IO ()        -- Will go to play or continueafterzerohelper
-- Base case: no more adjacent cells to investigate for 0 vals
continueafterzero [] state size board = play state size board
-- Investigate whether the first cell in todo has 0 val
continueafterzero (h:t) state size board =
 continueafterzerohelper h
                         (haskellminesweeper (Uncover h) state)
                         t
                         state
                         size
                         board

-- Called by continueafterzero, mainly to see result, which holds val. If it is
-- 0, it must be cleared.
continueafterzerohelper :: Coordinate   -- Cell that was investigated
                        -> Result       -- Result if that cell were cleared
                        -> [Coordinate] -- Cells still needing investigation
                        -> State        -- Old state. We keep this because the
                                        -- state in Result should not update
                                        -- game if it does not return a 0 val.
                        -> Int          -- Size of board
                        -> [Char]       -- String representation of board so far
                        -> IO ()        -- Will go back to continueafterzero
-- Cleared a 0 val
continueafterzerohelper h (ContinueAfterClear 0 (State u c f m)) t _ size oldboard =
 -- Add all adjacent cells of this newly cleared cell to todo, use state from
 -- this result, and update board.
 continueafterzero (t ++ [e | e <- todo, not (elem e u) && not (elem e t)])
                   (State u c f m)
                   size
                   (printnewboard h '0' oldboard)
 where todo = (getadjacentcoords h size)
-- Cleared a non-0 val
continueafterzerohelper _ _ t state size oldboard = continueafterzero t state size oldboard

