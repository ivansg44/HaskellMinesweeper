module Mines where
import System.Random
import HaskellMinesweeper
import Data.List
    


-- Generates a list of tuple coordinates of the entire grid of size n 
gridLocations :: Int -> [Coordinate]
gridLocations n = [(x,y) | x <- [0..n-1], y <- [0..n-1]]


-- Generates random list of indices numbers that fit inside the grid
randomIndices :: Int                -- gridSize
               -> Int               -- number of Indexes (mines)
               -> IO [Int]          -- list of indexes
randomIndices _ 0 = return []
randomIndices gridSize num =
    do
        seed <- newStdGen                                               -- generate random "seed"
        let infList = randomRs (0, gridSize*gridSize-1) seed            -- randomRs create infinite list given a "seed" value g,
        return (take num (nub infList))                                 -- nub removes duplicates, then take the necessary amount
                                                                            


-- Returns gridLocations at indicated indices
returnMineLocation :: [Int]            -- List of Indexes to check
                   -> [Coordinate]     -- List of gridLocations to check
                   -> [Coordinate]     -- Outputed gridLocations at said indexes
returnMineLocation (x:xs) grid
    |null xs = [grid !! x]
    |(x:xs) /= [] =  grid !! x : returnMineLocation xs grid
    |otherwise = []
    



-- Function to create a random mine positions list
randomMineLocations :: Int             -- gridSize
                    -> Int             -- number of mines
                    -> IO [Coordinate] -- list of mine locations
randomMineLocations gridSize num =
    do
        indices <- randomIndices gridSize num
        return (returnMineLocation indices (gridLocations gridSize))


-- Creates initial game state of 4 [Coordinate]
-- Uncovered == []
-- Covered == All cells not mines
-- Flagged == []
-- Mines == Generated random mines
initialState :: Int                 --gridSize      
                  -> Int            --mine amount   
                  -> IO State       --Starting state
initialState gridSize num = 
    do
        mines <- randomMineLocations gridSize num
        let covered = gridLocations gridSize \\ mines
        return (State [] covered [] mines)

