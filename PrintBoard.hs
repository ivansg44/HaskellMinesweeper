--TODO document
--idea: in IO we start with printinitboard, then when user does something, we 
--      use information from their action and the result to printnewboard. We 
--      need to keep track of the old board string in IO. TODO
module PrintBoard where

import HaskellMinesweeper

printinitboard :: Int -> [Char]
printinitboard size = foldl (\ acc e -> acc ++ ['\n'] ++ e) h t
 where (h:t) = printinitboardrows size

printinitboardrows :: Int -> [[Char]]
printinitboardrows size = [printinitboardrow size | _ <- [1..size]]

printinitboardrow :: Int -> [Char]
printinitboardrow size = ['-' | _ <- [1..size]]

{--
Test cases

-- TODO will not work with 0

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

printnewboard :: Coordinate -> Char -> [Char] -> [Char]
printnewboard coord newval oldboard =
 printnewboardhelper coord newval oldboard 0 0

printnewboardhelper :: Coordinate -> Char -> [Char] -> Int -> Int -> [Char]
printnewboardhelper _ _ [] _ _ = []
printnewboardhelper coord newval (h:t) xacc yacc
 | coord == (xacc,yacc) = newval:t
 | h == '\n'            = h:(printnewboardhelper coord newval t 0 (yacc+1))
 | otherwise            = h:(printnewboardhelper coord newval t (xacc+1) yacc)

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

