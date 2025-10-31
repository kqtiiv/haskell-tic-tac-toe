module Player where

data Player = O | X deriving (Eq, Ord, Show)

-- | Returns the other player
swap :: Player -> Player
swap X = O
swap O = X

-- | The player who starts the game
startingPlayer :: Player
startingPlayer = X
