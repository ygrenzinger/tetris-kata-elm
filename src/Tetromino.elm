module Tetromino exposing (..)

import Shape as S exposing (Shape, ShapeColor, TetrominoShape)


type Tetromino
    = Tetromino TetrominoShape ( Int, Int )


type TetrominoCommand
    = Move MoveCommand
    | Rotate RotateCommand


getColor : Tetromino -> ShapeColor
getColor (Tetromino shape _) =
    S.getColor shape


type MoveCommand
    = MoveDown
    | MoveLeft
    | MoveRight


type RotateCommand
    = RotateLeft
    | RotateRight


type WallKick
    = LeftWallKick Int
    | RightWallKick Int


applyCommand : TetrominoCommand -> Tetromino -> Tetromino
applyCommand command =
    case command of
        Move MoveDown ->
            moveTetrominoDown

        Move MoveLeft ->
            moveTetrominoLeft

        Move MoveRight ->
            moveTetrominoRight

        Rotate RotateLeft ->
            rotateTetrominoLeft

        Rotate RotateRight ->
            rotateTetrominoRight


moveTetrominoDown : Tetromino -> Tetromino
moveTetrominoDown (Tetromino shape ( i, j )) =
    Tetromino shape ( i + 1, j )


moveTetrominoLeft : Tetromino -> Tetromino
moveTetrominoLeft (Tetromino shape ( i, j )) =
    Tetromino shape ( i, j - 1 )


moveTetrominoRight : Tetromino -> Tetromino
moveTetrominoRight (Tetromino shape ( i, j )) =
    Tetromino shape ( i, j + 1 )


rotateTetrominoRight : Tetromino -> Tetromino
rotateTetrominoRight (Tetromino shape ( i, j )) =
    Tetromino (S.rotateClockWise shape) ( i, j )


rotateTetrominoLeft : Tetromino -> Tetromino
rotateTetrominoLeft (Tetromino shape ( i, j )) =
    Tetromino (S.rotateCounterClockWise shape) ( i, j )


positions : Tetromino -> List ( Int, Int )
positions (Tetromino shape ( i, j )) =
    List.map (\( ii, jj ) -> ( ii + i, jj + j )) (S.cellPositions shape)


whichWallKickToAttempt : Tetromino -> Maybe WallKick
whichWallKickToAttempt (Tetromino shape ( _, j )) =
    if j < 0 then
        Just (LeftWallKick (negate j))

    else if (j + S.shapeSize shape) >= 9 then
        Just (RightWallKick (j + S.shapeSize shape - 10))

    else
        Nothing


samePosition : Tetromino -> Tetromino -> Bool
samePosition (Tetromino _ ( a, b )) (Tetromino _ ( c, d )) =
    a == c && b == d
