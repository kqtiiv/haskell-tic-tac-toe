import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.HUnit (testCase, Assertion)
import IC.Exact ((-->))

import TicTacToe
import Board
import Player
import Cell

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tictactoe"
  [ testGroup "emptyBoard" (numberedTests emptyBoardTests)
  , testGroup "gameOver" (numberedTests gameOverTests)
  , testGroup "parsePosition" (numberedTests parsePositionTests)
  , testGroup "tryMove" (numberedTests tryMoveTests)
  ]

emptyBoardTests :: [Assertion]
emptyBoardTests = [ emptyBoard 1 --> Board 1 [Empty]
                  , emptyBoard 2 --> Board 2 [Empty, Empty, Empty, Empty]
                  , emptyBoard 3 --> Board 3 [ Empty, Empty, Empty
                                             , Empty, Empty, Empty
                                             , Empty, Empty, Empty
                                             ]
                  ]

gameOverTests :: [Assertion]
gameOverTests = [ gameOver testBoard1 --> True
                , gameOver testBoard2 --> False
                , gameOver testBoard3 --> True
                ]

parsePositionTests :: [Assertion]
parsePositionTests = [ parsePosition "0 2" --> Just (0,2)
                     , parsePosition "0 -8" --> Just (0,-8)
                     , parsePosition "-4 1" --> Just (-4,1)
                     , parsePosition "0 %1" --> Nothing
                     , parsePosition "" --> Nothing
                     , parsePosition "1 2 3" --> Nothing
                     ]

tryMoveTests :: [Assertion]
tryMoveTests = [ tryMove X (0,0) testBoard2 --> Nothing
               , tryMove O (-1,2) testBoard2 --> Nothing
               , tryMove O (0,-1) testBoard2 --> Nothing
               , tryMove O (1,1) testBoard2
                   --> Just (Board 2 [ Taken X,Empty
                                     , Empty,  Taken O
                                     ])
               , tryMove O (3,3) testBoard1
                   --> Just (Board 4 [ Taken O, Taken X, Empty,   Taken O
                                     , Taken O, Empty,   Taken X, Taken X
                                     , Taken O, Empty,   Empty,   Taken X
                                     , Taken O, Taken X, Empty,   Taken O
                                     ])
               ]

-------------------------------------------------------------------------------
-- HELPERS

{-|
This function just matches up a bunch of assertions to a numerical naming
system, allowing us to distinguish them.

If we wanted, we could provide descriptions to them instead...
-}
numberedTests :: [Assertion] -> [TestTree]
numberedTests = zipWith (\n -> testCase ("#" ++ show n)) ([1..] :: [Integer])
