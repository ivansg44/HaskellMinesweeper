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

-- startstate = State []
--                    [      (0,1),(0,2),(0,3),
--                     (1,0),(1,1),(1,2),(1,3),
--                     (2,0),(2,1),(2,2),(2,3),
--                     (3,0),(3,1),(3,2),(3,3)]
--                    []
--                    [(0,0)]

start :: IO ()
start =
 do
  let initboard = printinitboard 10
  mines <- initialState 10 10
  play mines 10 initboard

play :: State -> Int -> [Char] -> IO ()
play state size oldboard =
 do
  putStrLn oldboard
  action <- askforaction state 10
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
continue coord (ContinueAfterClear val (State u c f m)) size oldboard =
 if val == 0 then
  continueafterzero [e | e <- todo, not (elem e u)]
                    (State u c f m)
                    size
                    (printnewboard coord '0' oldboard)
 else
  play (State u c f m) size (printnewboard coord (head (show val)) oldboard)
 where todo = (getadjacentcoords coord size)
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

continueafterzero :: [Coordinate] -> State -> Int -> [Char] -> IO ()
continueafterzero [] state size board = play state size board
continueafterzero (h:t) state size board =
 continueafterzerohelper h
                         (haskellminesweeper (Uncover h) state)
                         t
                         state
                         size
                         board

continueafterzerohelper :: Coordinate
                        -> Result
                        -> [Coordinate]
                        -> State
                        -> Int
                        -> [Char]
                        -> IO ()
continueafterzerohelper h (ContinueAfterClear 0 (State u c f m)) t _ size oldboard =
 continueafterzero (t ++ [e | e <- todo, not (elem e u) && not (elem e t)])
                   (State u c f m)
                   size
                   (printnewboard h '0' oldboard)
 where todo = (getadjacentcoords h size)
continueafterzerohelper _ _ t state size oldboard = continueafterzero t state size oldboard

