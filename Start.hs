-- TODO document
module Start where

import AskForAction
import HaskellMinesweeper
import PrintBoard

startstate = State [      (0,1),(0,2),(0,3),
                    (1,0),(1,1),      (1,3),
                    (2,0),(2,1),(2,2),(2,3),
                    (3,0),(3,1),      (3,3)]
                   []
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
  action <- askforaction 4
  result <- return (haskellminesweeper action state)
  continue (getcoords action) result size oldboard

-- TODO in action verify flag isn't on uncovered (can pass state)
-- TODO end game (more patterns in continue)
-- TODO clear adjacent cells

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

