module Tetromino exposing (..)

import Shape exposing (Shape, cellPositions, rotateClockWise, rotateCounterClockWise, shapeSize)

type Tetromino = Tetromino Shape (Int, Int)
type TetrominoCommand = Move MoveCommand | Rotate RotateCommand
type MoveCommand = MoveDown | MoveLeft | MoveRight
type RotateCommand = RotateLeft | RotateRight

type WallKick = LeftWallKick Int | RightWallKick Int

moveTetrominoDown : Tetromino -> Tetromino
moveTetrominoDown (Tetromino shape (i,j)) = (Tetromino shape (i + 1,j))

moveTetrominoLeft : Tetromino -> Tetromino
moveTetrominoLeft (Tetromino shape (i,j)) = (Tetromino shape (i,j - 1))

moveTetrominoRight : Tetromino -> Tetromino
moveTetrominoRight (Tetromino shape (i,j)) = (Tetromino shape (i,j + 1))

rotateTetrominoRight : Tetromino -> Tetromino
rotateTetrominoRight (Tetromino shape (i,j)) = (Tetromino (rotateClockWise shape) (i,j))

rotateTetrominoLeft : Tetromino -> Tetromino
rotateTetrominoLeft (Tetromino shape (i,j)) = (Tetromino (rotateCounterClockWise shape) (i,j))

tetrominoPositions : Tetromino -> List (Int, Int)
tetrominoPositions (Tetromino shape (i,j)) = List.map (\(ii,jj) -> (ii + i, jj + j))  (cellPositions shape)

whichWallKickToAttempt : Tetromino -> Maybe WallKick
whichWallKickToAttempt (Tetromino shape (_,j)) =
    if j < 0
        then Just (LeftWallKick (negate j))
        else if ((j + (shapeSize shape)) >= 9)
            then Just (RightWallKick (j + (shapeSize shape) - 10))
            else Nothing