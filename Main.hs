module Main where

import Data.List (intersperse)

data Mark = X | O | E

data Board = Board [[Mark]]

data GameState =
  GameState {
    board :: Board
  }


instance Show Mark where
  show X = "X"
  show O = "O"
  show E = "."

instance Show Board where
  show (Board rows) = concat . intersperse "\n" . map showRow $ rows
    where showRow = intersperse ' ' . concat . map show

testBoard =
  Board [
    [X, O, E],
    [X, O, E],
    [X, O, E]
  ]

main :: IO ()
main = do
  putStrLn "Hello World"
