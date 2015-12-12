module Main where

import Data.List (intersperse, transpose)

data Mark = X | O | E deriving Eq

data Board = Board [[Mark]]

data Player = PlayerX | PlayerO

data GameState =
    GameDraw Board
  | GameWon Board Player
  | GameContinue Board Player

type Position = (Int, Int)

instance Show Mark where
  show X = "X"
  show O = "O"
  show E = "."

instance Show Board where
  show (Board rows) = concat . intersperse "\n" . map showRow $ rows
    where showRow = intersperse ' ' . concat . map show

readPosition :: String -> Either String Position
readPosition ('A' : n) = readInt n >>= \n -> Right (1, n)
readPosition ('B' : n) = readInt n >>= \n -> Right (2, n)
readPosition ('C' : n) = readInt n >>= \n -> Right (3, n)
readPosition (col : _) = Left ("Invalid column: " ++ [col])
readPosition position  = Left ("Invalid position: " ++ position)

readInt :: String -> Either String Int
readInt "1" = Right 1
readInt "2" = Right 2
readInt "3" = Right 3
readInt row = Left ("Invalid row: " ++ row)

getMark :: Player -> Mark
getMark PlayerX = X
getMark PlayerO = O

next :: Player -> Player
next PlayerX = PlayerO
next PlayerO = PlayerX

zipWithIndex :: (Enum a, Num a) => (a -> b -> c) -> [b] -> [c]
zipWithIndex fn xs = zipWith fn [1..] xs

addMark :: Position -> Mark -> Board -> Either String Board
addMark (col, row) mark (Board rows) = fmap Board . sequence . zipWithIndex mapRow $ rows
  where
    mapRow i row'
     | i == row  = sequence $ zipWithIndex mapCol row'
     | otherwise = Right row'
    mapCol j mark'
     | j == col && mark' /= E = Left "Position taken. Try again."
     | j == col && mark' == E = Right mark
     | otherwise              = Right mark'

hasWinner :: Board -> Mark -> Bool
hasWinner (Board rows) mark =
  any ($ rows) [
    any checkRow,
    any checkRow . transpose,
    checkRow . diagonal [0,1,2],
    checkRow . diagonal [2,1,0]
  ]
  where
    checkRow = all (== mark)
    diagonal = flip $ zipWith (!!)

isFull :: Board -> Bool
isFull (Board rows) = all (/= E) $ concat rows

emptyBoard = Board [[E,E,E], [E,E,E], [E,E,E]]
initialState = GameContinue emptyBoard PlayerX

placeMark :: String -> GameState -> Either String GameState
placeMark line (GameContinue board player) =
  do
    position <- readPosition line
    newBoard <- addMark position (getMark player) board
    return $ decideWinner newBoard
  where
    decideWinner board =
      if hasWinner board X || hasWinner board O
      then GameWon board player
      else
        if isFull board
        then GameDraw board
        else GameContinue board (next player)

announceVictor PlayerX = "X has won"
announceVictor PlayerO = "O has won"
announceDraw = "It's a draw!"

showState :: GameState -> IO ()
showState gameState = do
  case gameState of
    GameContinue board player -> putStrLn (show board)
    GameWon board player      -> putStrLn (show board) >> putStrLn (announceVictor player)
    GameDraw board            -> putStrLn (show board) >> putStrLn announceDraw

readLine :: String -> IO String
readLine prompt = putStr prompt >> getLine

nextState :: GameState -> IO GameState
nextState gameState @ (GameContinue board player) = do
  putStrLn "———————————————————————————————————————————————————————————————————"
  line <- readLine "position: "
  putStrLn "———————————————————————————————————————————————————————————————————"
  case placeMark line gameState of
    Left msg        -> putStrLn msg >> return (GameContinue board player)
    Right gameState -> showState gameState >> return gameState

iterateUntilM :: (Monad m) => (a -> Bool) -> (a -> m a) -> a -> m a
iterateUntilM p f v
  | p v       = return v
  | otherwise = f v >>= iterateUntilM p f

hasFinished :: GameState -> Bool
hasFinished (GameDraw _)       = True
hasFinished (GameWon _ _)      = True
hasFinished (GameContinue _ _) = False

main :: IO GameState
main = do
  putStrLn "Hello!"
  putStrLn "Let's play Tic-Tac-Toe!"
  iterateUntilM hasFinished nextState initialState
