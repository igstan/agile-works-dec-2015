module Main where

data Mark = X | O | Empty

type Board = [[Mark]]

data GameState =
  GameState {
    board :: Board
  }

testBoard =
  [
    [X, O, Empty],
    [X, O, Empty],
    [X, O, Empty]
  ]

main :: IO ()
main = do
  putStrLn "Hello World"
