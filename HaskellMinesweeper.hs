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


-------------------------------------------------------------------------------------------------------------------------

-- Generates a list of tuple coordinates of the entire grid of size n 
gridLocations :: Int                -- Grid Size
                -> [Coordinate]     -- Output gridLocations
gridLocations n = [(x,y) | x <- [0..n-1], y <- [0..n-1]]

-- Genertes random list of numbers that lie within 0-total grid squares
randomIndices :: Int                -- gridSize
               -> Int               -- number of Indexes (mines)
               -> IO [Int]          -- list of indexes
randomIndices _ 0 = return []
randomIndices gridSize num =
    do
        g <- newStdGen                                                      -- generate random "seed"
        return $ take num $ nub $ randomRs (0, gridSize*gridSize - 1) g     -- randomRs create infinite list given a "seed" value g,
                                                                            -- nub removes duplicates, then take the necessary amount
                                                                            


-- Returns gridLocations at indicated indexes
returnBombLocation :: Int                -- Counter to make sure we reached end of list
                  -> [Int]             -- List of Indexes to check
                  -> [Coordinate]       -- List of gridLocations to check
                  -> [Coordinate]      -- Outputed gridLocations at said indexes
returnBombLocation a indexes positions
    | a /= length indexes = positions !! (indexes !! a) : returnBombLocation (a+1) indexes positions
    | otherwise = []        

-- Function to create a random mine positions list
randomBombLocations :: Int             -- gridSize
                  -> Int               -- number of mines
                  -> IO [Coordinate]   -- list of bomb locations
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


--------------------------------------------------------------------------------------------------------------

--Test board sizes can change later
easyGameBoard = 5
mediumGameBoard = 7
hardGameBoard = 10

-- IDK might be useful, I was using this to test things out before
generateState :: [Coordinate] -> [Coordinate] -> [Coordinate] -> [Coordinate] -> State
generateState  = State



-- Essentially bomb generation code,
initialState :: Int            
                  -> Int               
                  -> IO State  
initialState gridSize num = 
    do
        indexes <- randomIndices gridSize num
        return $ State [] (gridLocations gridSize) [] (returnBombLocation 0 indexes $ gridLocations gridSize)


-- Test
--  exampleStart = initialState 3 3
-- savedStart = exampleStart

-- Issue: You cannot "save" a random number
-- store a random number to a variable
-- run that variable multiple times and we get multiple answers.
