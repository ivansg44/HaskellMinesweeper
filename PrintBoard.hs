-- Prints a board representing the game.
-- This works independent of the game framework. We must modify both the game
-- and a string representation of the board in Start.hs.
module PrintBoard where

import HaskellMinesweeper
import Mines

-- Print initial board.
-- Gets a list of identical rows from printinitboardrows, and then concatenates
-- them into a single string with a '\n' delimiter.
printinitboard :: Int    -- Size of initial board
               -> [Char] -- String representation of initial board
printinitboard size = foldl (\ acc e -> acc ++ ['\n'] ++ e) h t
 where (h:t) = printinitboardrows size

-- Get a list of rows in the initial board
printinitboardrows :: Int      -- Size of init board i.e., how many rows needed
                   -> [[Char]] -- List of identical rows that will be
                               -- concatenated into a single string in
                               -- printinitboard.
printinitboardrows size = [printinitboardrow size | _ <- [1..size]]

-- Get a single row in initial board
printinitboardrow :: Int   -- Size of init board i.e., how long row should be
                 -> [Char] -- "-----"... as long as needed
printinitboardrow size = ['-' | _ <- [1..size]]

{--
Test cases

putStrLn(printinitboard 1)
==
-

putStrLn(printinitboard 2)
==
--
--

putStrLn(printinitboard 3)
==
---
---

--}

-- Updates existing board string at specific coordinate with new character
printnewboard :: Coordinate -- Coordinate of board string to update
              -> Char       -- Character to place at that coordinate
              -> [Char]     -- Old board string
              -> [Char]     -- Updated board string
printnewboard coord newval oldboard =
 printnewboardhelper coord newval oldboard 0 0

-- Even though the string does not have any built-in coordinates, we pretend it
-- does, by imagining the string is split into row by the '\n' character, and
-- the characters in each row belong to a different column. This helper uses
-- two Int values as x and y coordinate accumulators, until it reaches the
-- coordinate to modify.
printnewboardhelper :: Coordinate -> Char -> [Char] -> Int -> Int -> [Char]
printnewboardhelper _ _ [] _ _ = []
printnewboardhelper coord newval (h:t) xacc yacc
 -- This is the coordinate. Replace it.
 | coord == (xacc,yacc) = newval:t
 -- This is not the coordinate, but move to a new row.
 | h == '\n'            = h:(printnewboardhelper coord newval t (xacc+1) 0)
 -- This is not the coordinate. Move to the next column in this row.
 | otherwise            = h:(printnewboardhelper coord newval t xacc (yacc+1))

{---
Test cases

putStrLn(printnewboard (0,0) '3' "---\n---")
==
3--
---

putStrLn(printnewboard (1,1) '#' "---\n---")
==
---
-#-

putStrLn(printnewboard (0,2) '!' "---\n-2-")
==
--!
-2-

--}

