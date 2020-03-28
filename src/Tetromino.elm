module Tetromino exposing (..)

import Shape exposing (Shape, cellPositions)

type Tetromino = Tetromino Shape (Int, Int)
type TetrominoCommand = MoveDown | MoveLeft | MoveRight

moveTetrominoDown : Tetromino -> Tetromino
moveTetrominoDown (Tetromino shape (i,j)) = (Tetromino shape (i + 1,j))

moveTetrominoLeft : Tetromino -> Tetromino
moveTetrominoLeft (Tetromino shape (i,j)) = (Tetromino shape (i,j - 1))

moveTetrominoRight : Tetromino -> Tetromino
moveTetrominoRight (Tetromino shape (i,j)) = (Tetromino shape (i,j + 1))

tetrominoPositions : Tetromino -> List (Int, Int)
tetrominoPositions (Tetromino shape (i,j)) = List.map (\(ii,jj) -> (ii + i, jj + j))  (cellPositions shape)
