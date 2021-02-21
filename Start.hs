-- TODO document
module Start where

import AskForAction
import HaskellMinesweeper
import PrintBoard

startstate = State []
                   [      (0,1),(0,2),(0,3),
                    (1,0),(1,1),      (1,3),
                    (2,0),(2,1),(2,2),(2,3),
                    (3,0),(3,1),      (3,3)]
                   []
                   [(0,0),                 
                                (1,2),     
                                           
                                (3,2)      ]

start :: IO ()
start =
 do
  let initboard = printinitboard 4
  play startstate 4 initboard

play :: State -> Int -> [Char] -> IO ()
play state size oldboard =
 do
  putStrLn oldboard
  action <- askforaction state 4
  result <- return (haskellminesweeper action state)
  if endgame result then
   finish result state oldboard
  else
   continue (getcoords action) result size oldboard

-- TODO clear adjacent cells

finish :: Result -> State -> [Char] -> IO ()
finish (EndOfGame win) oldstate oldboard =
 do
  if win then
   printwinnerboard oldstate oldboard
  else
   printloserboard oldstate oldboard

printwinnerboard :: State -> [Char] -> IO ()
printwinnerboard (State _ [last] _ mines) oldboard =
 do
  lastval <- return (countmines last mines)
  new <- return (printnewboard last (head (show lastval)) oldboard)
  putStrLn new
  putStrLn "You win."

printloserboard :: State -> [Char] -> IO ()
printloserboard (State _ _ _ mines) oldboard =
 do
  new <- return (foldr (\ e acc -> printnewboard e '#' acc) oldboard mines)
  putStrLn new
  putStrLn "You lose."

continue :: Coordinate -> Result -> Int -> [Char] -> IO ()
continue coord (ContinueAfterClear val state) size oldboard =
 play state size (printnewboard coord (head (show val)) oldboard)
continue coord
         (ContinueAfterFlag (State uncovered covered flagged mines))
         size
         oldboard =
 if elem coord flagged then
  play (State uncovered covered flagged mines)
       size
       (printnewboard coord '!' oldboard)
 else
  play (State uncovered covered flagged mines)
       size
       (printnewboard coord '-' oldboard)

