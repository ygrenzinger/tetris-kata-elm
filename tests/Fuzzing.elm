module Fuzzing exposing (..)

import Array
import Fuzz exposing (Fuzzer, constant, frequency, int, intRange, list, map)
import Shape exposing (Shape, TetrominoShape, allShapes, shapeI)
import Tetris exposing (Tetris, TetrisCommmand(..))
import Tetromino exposing (MoveCommand(..), RotateCommand(..), TetrominoCommand(..))


retrieveShape : Int -> TetrominoShape
retrieveShape i =
    let
        shapesArray =
            Array.fromList allShapes
    in
    Array.get (modBy (Array.length shapesArray) i) shapesArray |> Maybe.withDefault shapeI


fuzzShape : Fuzzer TetrominoShape
fuzzShape =
    int |> map retrieveShape


chooseMoveCommand : Int -> MoveCommand
chooseMoveCommand i =
    case i of
        0 ->
            MoveLeft

        1 ->
            MoveRight

        _ ->
            MoveDown


fuzzMoveCommand : Fuzzer MoveCommand
fuzzMoveCommand =
    intRange 0 2 |> map chooseMoveCommand


fuzzTetrominoCommand : Fuzzer TetrominoCommand
fuzzTetrominoCommand =
    frequency
        [ ( 5, constant (Move MoveRight) )
        , ( 4, constant (Move MoveLeft) )
        , ( 2, constant (Rotate RotateRight) )
        , ( 2, constant (Rotate RotateLeft) )
        ]


fuzzTetrisAction : Fuzzer (List TetrisCommmand)
fuzzTetrisAction =
    frequency
        [ ( 9, list fuzzTetrominoCommand |> Fuzz.map (List.map TetrominoCommand) )
        , ( 1, fuzzShape |> Fuzz.map (\s -> [ TetrominoCommand Drop, FallingDown, SpawningTetromino ( s, [] ) ]) )
        ]
