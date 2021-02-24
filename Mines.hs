module Mines where
import System.Random
import HaskellMinesweeper
import Data.List
    
    -- Generates a list of tuple coordinates of the entire grid of size n 
gridLocations :: Int -> [Coordinate]
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