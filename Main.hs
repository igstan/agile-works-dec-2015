module Main where

import Data.List (intersperse, transpose)

data Mark = X | O | E deriving Eq

data Board = Board [[Mark]]

data GameState =
  GameState {
    board :: Board
  }

type Position = (Int, Int)

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

readPosition :: String -> Either String Position
readPosition ('A' : n) = readInt n >>= \n -> Right (1, n)
readPosition ('B' : n) = readInt n >>= \n -> Right (2, n)
readPosition ('C' : n) = readInt n >>= \n -> Right (3, n)
readPosition (col : _) = Left ("Invalid column: " ++ [col])
readPosition pos       = Left ("Invalid position: " ++ pos)

readInt :: String -> Either String Int
readInt "1" = Right 1
readInt "2" = Right 2
readInt "3" = Right 3
readInt r   = Left ("Invalid row: " ++ r)


addMark :: Position -> Mark -> Board -> Either String Board
addMark (col, row) mark (Board rows) = fmap Board $ sequence $ zipWith mapRow [1..] rows
  where
     mapRow :: Int -> [Mark] -> Either String [Mark]
     mapRow i row' = sequence $ zipWith (mapCol i) [1..] row'
     mapCol :: Int -> Int -> Mark -> Either String Mark
     mapCol i j mark' | i == row && j == col && mark' == E = Right mark
                      | i == row && j == col && mark' /= E = Left "Position is non-empty"
                      | otherwise                          = Right mark'

hasWinner :: Board -> Bool
hasWinner (Board rows) =
  or [
    checkRows rows,
    checkRows . transpose $ rows,
    checkRows [diagonal rows [0,1,2]],
    checkRows [diagonal rows [2,1,0]]
  ]
  where
    checkRows = any (\row -> any ($ row) [all (==X), all (==O)])
    diagonal = zipWith (!!)

isFull :: Board -> Bool
isFull = undefined

main :: IO ()
main = do
  putStrLn "Hello World"
