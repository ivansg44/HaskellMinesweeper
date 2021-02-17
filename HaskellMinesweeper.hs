-- TODO document file
module HaskellMinesweeper where

data State = State [Coordinate] -- Uncovered cells
                   [Coordinate] -- Covered cells
                   [Coordinate] -- Flagged cells (can overlap with covered
                                -- and mines).
                   [Coordinate] -- Mines
  deriving (Show)

data Result = EndOfGame Bool               -- True means victory
            | ContinueAfterFlag State      -- Next state
            | ContinueAfterClear Int State -- Number of adjacent mines and next
                                           -- state.
  deriving (Show)

type Game = Action -> State -> Result

data Action = Flag Coordinate    -- Flag or unflag a cell
            | Uncover Coordinate -- Uncover a cell

-- Cell x y coordinates
type Coordinate = (Int, Int)

haskellminesweeper :: Game
haskellminesweeper (Uncover (x,y)) (State uncovered covered flagged mines)
 | elem (x,y) mines   = EndOfGame False
 | [(x,y)] == covered = EndOfGame True
 | otherwise          =
    ContinueAfterClear 0 (State [] [] [] mines) -- TODO finish
haskellminesweeper (Flag (x,y)) (State uncovered covered flagged mines)
 | elem (x,y) flagged =
    ContinueAfterFlag (State uncovered
                             covered
                             [e | e <- flagged, e /= (x,y)]
                             mines)
 | otherwise          =
    ContinueAfterFlag (State uncovered covered ((x,y):flagged) mines)

{--
Test cases

TODO we can reject illegal moves in IO

haskellminesweeper (Uncover (1,0)) (State [(0,0),(0,1)] [(1,1)] [] [(1,0)])
== EndOfGame False

haskellminesweeper (Uncover (1,1)) (State [(0,0),(0,1)] [(1,1)] [] [(1,0)])
== EndOfGame True

haskellminesweeper (Flag (1,0)) (State [(0,0),(0,1)] [(1,1)] [] [(1,0)])
== ContinueAfterFlag (State [(0,0),(0,1)] [(1,1)] [(1,0)] [(1,0)]

haskellminesweeper (Flag (1,1)) (State [(0,0),(0,1)] [(1,1)] [] [(1,0)])
== ContinueAfterFlag (State [(0,0),(0,1)] [(1,1)] [(1,1)] [(1,0)])

haskellminesweeper (Flag (1,1)) (State [(0,0),(0,1)] [(1,1)] [(1,1)] [(1,0)])
== ContinueAfterFlag (State [(0,0),(0,1)] [(1,1)] [] [(1,0)])

haskellminesweeper (Uncover (1,1)) (State [(0,0),(0,1)] [] [(1,1)] [(1,0)])
== EndOfGame True

haskellminesweeper (Uncover (2,2)) (State [(0,1)] [(0,2),(1,0),(1,1),(1,2),(2,0),(2,1),(2,2)] [] [(0,0)])
== ContinueAfterClear 0 (State [(2,2),(0,1)] [(0,2),(1,0),(1,1),(1,2),(2,0),(2,1)] [] [(0,0)])

haskellminesweeper (Uncover (1,1)) (State [(0,1)] [(0,2),(1,0),(1,1),(1,2),(2,0),(2,1),(2,2)] [] [(0,0)])
== ContinueAfterClear 1 (State [(1,1),(0,1)] [(0,2),(1,0),(1,2),(2,0),(2,1),(2,2)] [] [(0,0)])

haskellminesweeper (Uncover (1,1)) (State [(0,1)] [(0,2),(1,1),(2,0),(2,1),(2,2)] [] [(0,0),(1,0),(1,2)])
== ContinueAfterClear 3 (State [(1,1),(0,1)] [(0,2),(2,0),(2,1),(2,2)] [] [(0,0),(1,0),(1,2)])

haskellminesweeper (Uncover (1,1) (State [] [(0,1),(1,1)] [] [(0,0),(0,2),(1,0),(1,2),(2,0),(2,1),(2,2)])
== ContinueAfterClear 7 (State [(1,1)] [(0,1)] [] [(0,0),(0,2),(1,0),(1,2),(2,0),(2,1),(2,2)])
--}

