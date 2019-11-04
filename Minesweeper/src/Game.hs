module Logic where

    --TODO implement game logic
import Data.Array

type Cell  = Maybe Int
type Board = Array (Int, Int) Cell
type State = Running | GameOver

data Game = { gameBoards :: Board
            , gameState :: State
            } deriving(Eq, Show)
-- n = boardDimensions
n :: Int
n = 10
-- game loop

-- setup
initialGame = Game { gameBoard = array indexRange $ zip (range indexRange) (repeat Nothing)
               , gameState = Running
               }
where indexRange = ((0, 0), (n - 1, n - 1))
    -- generate board
    -- place mines
