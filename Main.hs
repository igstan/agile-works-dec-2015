module Main where

import Data.List (intersperse, transpose)

data Mark = X | O | E deriving Eq

data Board = Board [[Mark]]

data Player = PlayerX | PlayerO

data GameState =
  GameState {
    board :: Board,
    player :: Player
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

getMark :: Player -> Mark
getMark PlayerX = X
getMark PlayerO = O

next :: Player -> Player
next PlayerX = PlayerO
next PlayerO = PlayerX

addMark :: Position -> Mark -> Board -> Either String Board
addMark (col, row) mark (Board rows) = fmap Board $ sequence $ zipWith mapRow [1..] rows
  where
     mapRow i row' = sequence $ zipWith (mapCol i) [1..] row'
     mapCol i j mark' | i == row && j == col && mark' == E = Right mark
                      | i == row && j == col && mark' /= E = Left "Position taken. Try again."
                      | otherwise                          = Right mark'

hasWinner :: Board -> Mark -> Bool
hasWinner (Board rows) mark =
  or [
    checkRows rows,
    checkRows . transpose $ rows,
    checkRows [diagonal rows [0,1,2]],
    checkRows [diagonal rows [2,1,0]]
  ]
  where
    checkRows = any $ all (== mark)
    diagonal = zipWith (!!)

isFull :: Board -> Bool
isFull (Board rows) = all (/= E) $ concat rows

emptyBoard = Board [[E,E,E], [E,E,E], [E,E,E]]

nextState :: String -> GameState -> Either String (String, Board)
nextState line gameState =
  do
    pos <- readPosition line
    newBoard <- addMark pos (getMark $ player gameState) (board gameState)
    let msg = if hasWinner newBoard X || hasWinner newBoard O
        then announceVictor $ player gameState
        else
          if isFull newBoard
          then announceDraw
          else show newBoard
    return (msg, newBoard)
  where
    announceVictor PlayerX = "X has won"
    announceVictor PlayerO = "O has won"
    announceDraw = "It's a draw!"

loop :: Board -> Player -> IO ()
loop board player =
  do
    line <- getLine
    result <- return $ nextState line (GameState board player)
    let (msg, newBoard, newPlayer) =
          case result of
            Left msg -> (msg, board, player)
            Right (msg, newBoard) -> (msg, newBoard, next player)
    putStrLn msg
    loop newBoard newPlayer

main :: IO ()
main =
  do
  putStrLn "Hello!"
  putStrLn "Let's play Tic-Tac-Toe!"
  loop emptyBoard PlayerX
