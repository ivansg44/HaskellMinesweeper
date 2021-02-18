-- Underlying game framework
module HaskellMinesweeper where
import System.Random

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
 | elem (x,y) mines   = EndOfGame False -- Uncovered a mine
 | [(x,y)] == covered = EndOfGame True  -- Uncovered last covered cell
 | otherwise          =
    ContinueAfterClear (countmines (x,y) mines)
                       (State ((x,y):uncovered)
                              [e | e <- covered, e /= (x,y)]
                              flagged
                              mines)
haskellminesweeper (Flag (x,y)) (State uncovered covered flagged mines)
 | elem (x,y) flagged = -- Unflag flagged cell
    ContinueAfterFlag (State uncovered
                             covered
                             [e | e <- flagged, e /= (x,y)]
                             mines)
 | otherwise          = -- Flag unflagged cell
    ContinueAfterFlag (State uncovered covered ((x,y):flagged) mines)

-- Counts the number of mines adjacent to some coordinate
countmines :: Coordinate   -- Coordinate to count adjacent mines for
           -> [Coordinate] -- Coordinates of all mines
           -> Int
countmines (x,y) m =
 foldr (\ (x1,y1) acc -> if areadjacent (x,y) (x1,y1) then acc+1 else acc) 0 m

-- Returns True if two coordinates are adjacent
areadjacent :: Coordinate -> Coordinate -> Bool
areadjacent (x,y) (x1,y1) =
 ((x1 >= (x-1)) && (x1 <= (x+1)) && (y1 >= (y-1)) && (y1 <= (y+1)))

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

haskellminesweeper (Uncover (1,1)) (State [(0,0),(0,1)] [(1,1)] [(1,1)] [(1,0)])
== EndOfGame True

haskellminesweeper (Uncover (2,2)) (State [(0,1)] [(0,2),(1,0),(1,1),(1,2),(2,0),(2,1),(2,2)] [] [(0,0)])
== ContinueAfterClear 0 (State [(2,2),(0,1)] [(0,2),(1,0),(1,1),(1,2),(2,0),(2,1)] [] [(0,0)])

haskellminesweeper (Uncover (1,1)) (State [(0,1)] [(0,2),(1,0),(1,1),(1,2),(2,0),(2,1),(2,2)] [] [(0,0)])
== ContinueAfterClear 1 (State [(1,1),(0,1)] [(0,2),(1,0),(1,2),(2,0),(2,1),(2,2)] [] [(0,0)])

haskellminesweeper (Uncover (1,1)) (State [(0,1)] [(0,2),(1,1),(2,0),(2,1),(2,2)] [] [(0,0),(1,0),(1,2)])
== ContinueAfterClear 3 (State [(1,1),(0,1)] [(0,2),(2,0),(2,1),(2,2)] [] [(0,0),(1,0),(1,2)])

haskellminesweeper (Uncover (1,1)) (State [] [(0,1),(1,1)] [] [(0,0),(0,2),(1,0),(1,2),(2,0),(2,1),(2,2)])
== ContinueAfterClear 7 (State [(1,1)] [(0,1)] [] [(0,0),(0,2),(1,0),(1,2),(2,0),(2,1),(2,2)])
--}



------------------------------------------------------------------------------------------------------------------
-- How would I be able to show/print my results on the terminal?

-- Cell x y coordinates
type Coordinate2 = (IO Int, IO Int)

-- Creates a tuple of IO Ints within the bounds of a grid size n
-- Random Integers are only possible through IO Int
randomSingleMine :: Int -> Coordinate2
randomSingleMine n = (randomRIO (0,n), randomRIO (0,n))

-- Creates a list of size = mineCount, and individual mines within the grid
-- Does not deal with duplicate mines as of yet
randomMineList :: Int -> Int -> [Coordinate2]
randomMineList mineCount gridSize
   |mineCount == 0 = []
   |otherwise = (randomSingleMine gridSize):randomMineList(mineCount - 1) gridSize

