-- Underlying game framework
module HaskellMinesweeper where
import System.Random
import Data.List

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
  deriving (Read,Show)

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

--TODO document this useful function
getcoords :: Action -> Coordinate
getcoords (Flag coord) = coord
getcoords (Uncover coord) = coord

--TODO document this useful function
endgame :: Result -> Bool
endgame (EndOfGame _) = True
endgame _             = False

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


-------------------------------------------------------------------------------------------------------------------------

-- Generates a list of tuple coordinates of the entire grid of size n 
gridLocations :: Int                -- Grid Size
                -> [(Int, Int)]     -- Output gridLocations
gridLocations n = [(x,y) | x <- [0..n-1], y <- [0..n-1]]

-- Genertes random list of numbers that lie within 0-total grid squares
randomIndices :: Int                -- gridSize
               -> Int               -- number of Indexes (mines)
               -> IO [Int]          -- list of indexes
randomIndices _ 0 = return []
randomIndices gridSize n = do
  r  <- randomRIO (0,gridSize*gridSize-1)
  rs <- randomIndices gridSize (n-1)
  return (r:rs) 


-- Returns gridLocations at indicated indexes
returnBombLocation :: Int                -- Counter to make sure we reached end of list
                  -> [Int]             -- List of Indexes to check
                  -> [(Int,Int)]       -- List of gridLocations to check
                  -> [(Int, Int)]      -- Outputed gridLocations at said indexes
returnBombLocation a indexes positions
    | a /= length indexes = positions !! (indexes !! a) : returnBombLocation (a+1) indexes positions
    | otherwise = []        

-- Function to create a random mine positions list
randomBombLocations :: Int             -- gridSize
                  -> Int               -- number of mines
                  -> IO [(Int, Int)]   -- list of bomb locations
randomBombLocations gridSize num = 
    do
        indexes <- randomIndices gridSize num
        return $ returnBombLocation 0 indexes $ gridLocations gridSize



-- Tests

-- exampleGrid = gridLocations 4
-- == [(0,0),(0,1),(0,2),(0,3),(1,0),(1,1),(1,2),(1,3),(2,0),(2,1),(2,2),(2,3),(3,0),(3,1),(3,2),(3,3)]

-- indices = randomIndices 4 4
-- indices == [different every time]

-- bombLocations = returnBombLocation  0 [0, 10, 1, 14] exampleGrid
-- [(0,0),(2,2),(0,1),(3,2)]

-- exampleRandomBombs = randomBombLocations 10 10 
-- exampleRandomBombs = [different every time]
