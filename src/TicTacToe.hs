module TicTacToe (gameOver, parsePosition, tryMove) where

import Text.Read (readMaybe)
import Data.Containers.ListUtils (nubOrd)
import Data.Char (isNumber)

import Control.Applicative
import Control.Monad

import Board
import Cell
import Player
import Helpers

type Position = (Int, Int)

gameOver :: Board -> Bool
gameOver b = checkLines allLines
    where 
        checkLines :: [[Cell]] -> Bool 
        checkLines ls = any hasWon (map nubOrd ls) 

        hasWon :: [Cell] -> Bool 
        hasWon [x] = not (isEmpty x)  -- won if singleton is not empty
        hasWon _ = False

        allLines = rows b ++ cols b ++ diags b 

--
-- Moves must be of the form "row col" where row and col are integers
-- separated by whitespace. Bounds checking happens in tryMove, not here.
--
parsePosition :: String -> Maybe Position
parsePosition xs = case words xs of 
                   [a, b] -> (,) <$> readMaybe a <*> readMaybe b
                   _      -> Nothing


tryMove :: Player -> Position -> Board -> Maybe Board
tryMove player (i, j) (Board n cells) = do 
    guard (i>=0 && i<n && j>=0 && j<n)      -- check bounds
    guard (isEmpty $ cells !! boardPos)     -- check if empty
    return (Board n updatedCells)
    where 
        boardPos = i * n + j
        updatedCells = replace boardPos (Taken player) cells
