-- TODO document file
module HaskellMinesweeper where

data State = State [Coordinate] -- Uncovered cells
                   [Coordinate] -- Flagged cells
                   [Coordinate] -- Mines
                   Int          -- # of cells in one row and one col
  deriving (Show)

data Result = EndOfGame Bool               -- True means victory
            | ContinueAfterFlag State      -- Next state
            | ContinueAfterClear Int State -- Number of adjacent mines and next
                                           -- state.
  deriving (Show)

type Game = Action -> State -> Result

data Action = Flag Coordinate    -- Flag a cell
            | Uncover Coordinate -- Uncover a cell

-- Cell x y coordinates
type Coordinate = (Int, Int)

haskellminesweeper :: Game
haskellminesweeper _ (State _ _ _ _) = EndOfGame False

{--
Test cases

TODO we can more explicitly reject illegal moves in IO
TODO I made it one-based index because it may be easier to compute

haskellminesweeper (Uncover (2,1)) (State [(1,1),(1,2)] [] [(2,1)] 2)
== EndOfGame False

haskellminesweeper (Uncover (2,2)) (State [(1,1),(1,2)] [] [(2,1)] 2)
== EndOfGame True

haskellminesweeper (Flag (2,2)) (State [(1,1),(1,2)] [] [(2,1)] 2)
== ContinueAfterFlag (State [(1,1),(1,2)] [(2,2)] [(2,1)] 2)

haskellminesweeper (Uncover (2,2)) (State [(1,1),(1,2)] [(2,2)] [(2,1)] 2)
== EndOfGame True

haskellminesweeper (Uncover (10,10)) (State [(1,1),(1,2)] [] [(2,1)] 10)
== ContinueAfterClear 0 (State [(10,10),(1,1),(1,2)] [] [(2,1)] 10)

haskellminesweeper (Uncover (1,1)) (State [] [] [(2,2)] 3)
== ContinueAfterClear 1 (State [(1,1)] [] [(2,2)] 3)

haskellminesweeper (Uncover (2,2)) (State [] [] [(1,1),(3,3)] 3)
== ContinueAfterClear 2 (State [(2,2)] [] [(1,1),(3,3)] 3)

haskellminesweeper (Uncover (2,2)) (State [] [] [(1,1),(1,2),(1,3),(2,1),(2,3),(3,1),(3,2),(3,3)] 3)
== ContinueAfterClear 8 (State [(2,2)] [] [(1,1),(1,2),(1,3),(2,1),(2,3),(3,1),(3,2),(3,3)] 3)

--}

