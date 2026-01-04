module Main where

import TicTacToe
import Board
import Cell
import Player
import Helpers

import Text.Read (readMaybe)
import Data.List (intersperse)
import Control.Monad

import System.IO (hSetBuffering, stdout, stdin, BufferMode(NoBuffering))

prettyPrint :: Board -> IO ()
prettyPrint b = putStrLn $ unlines (map joinRows rowList)
  where rowList = rows b
        joinRows :: [Cell] -> String
        joinRows = (intersperse ' ') . unwords . (map show)

-- The following reflect the suggested structure, but you can manage the game
-- in any way you see fit.

-- You might like to use one of the following signatures, as per the spec.
-- but this is up to you.
--
-- doParseAction :: (String -> Maybe a) -> IO a
-- doParseAction :: String -> (String -> Maybe a) -> IO a

-- better 
untilJustM :: Monad m => m (Maybe a) -> m a 
untilJustM mmse = mmse >>= maybe (untilJustM mmse) return 

doParseAction :: String -> (String -> Maybe a) -> IO a 
doParseAction msg valid = untilJustM $ valid <$> (putStrLn msg *> getLine)

-- | Repeatedly read a target board position and invoke tryMove until
-- the move is successful (Just ...).

takeTurn :: Board -> Player -> IO Board
takeTurn b p = do 
  putStrLn ("Player " ++ show p) 
  let prompt = "Enter the grid position in the form: row col"
  updatedBoard <- doParseAction prompt validate
  return updatedBoard
  where 
    validate s = do 
      pos <- parsePosition s
      move <- tryMove p pos b
      return move

-- | Manage a game by repeatedly: 1. printing the current board, 2. using
-- takeTurn to return a modified board, 3. checking if the game is over,
-- printing the board and a suitable congratulatory message to the winner
-- if so.
playGame :: Board -> Player -> IO ()
playGame b p = do 
  prettyPrint b 
  newBoard <- takeTurn b p
  if gameOver newBoard then 
    do 
      putStrLn "Game Over!"
      putStrLn ("Player " ++ show p ++ " won!")
      prettyPrint newBoard
  else 
    playGame newBoard (swap p)

-- | Print a welcome message, read the board dimension, invoke playGame and
-- exit with a suitable message.

main :: IO ()
main = do
  disableBuffering -- don't remove!
  putStrLn "Welcome to Tic Tac Toe!"
  let prompt = "Enter the board size (NxN): "
  n <- doParseAction prompt $ (filterMaybe (>0)).readMaybe
  playGame (emptyBoard n) startingPlayer
  return ()
  

{-|
When ran via `cabal run`, Haskell will "buffer" the input and output streams
for performance. This is annoying, since it means that some printing can happen
out-of-order -- the strings are only written when a newline is entered!

There are a few ways of getting around this, like using `hFlush stdout` after
each print. However, in this case, it is simplest to just disable the buffering
altogether. This function *must* be called at the start of your `main` function.
-}
disableBuffering :: IO ()
disableBuffering = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin NoBuffering
