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

readPosition :: String -> Either String (Int, Int)
readPosition ('A' : n : _) = readInt n >>= \n -> Right (1, n)
readPosition ('B' : n : _) = readInt n >>= \n -> Right (2, n)
readPosition ('C' : n : _) = readInt n >>= \n -> Right (3, n)
readPosition (col : _ : _) = Left ("Invalid column: " ++ [col])
readPosition pos           = Left ("Invalid position: " ++ pos)

readInt :: Char -> Either String Int
readInt '1' = Right 1
readInt '2' = Right 2
readInt '3' = Right 3
readInt r   = Left ("Invalid row: " ++ [r])


main :: IO ()
main = do
  putStrLn "Hello World"
