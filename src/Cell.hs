module Cell where

import Player (Player)

-- You may wish to define your own Show instance
data Cell = Empty | Taken Player deriving (Eq, Ord)

instance Show Cell where 
    show :: Cell -> String 
    show Empty = "-"
    show (Taken player) = show player

-- | Returns True if the given cell is Empty
isEmpty :: Cell -> Bool
isEmpty Empty = True
isEmpty _     = False
