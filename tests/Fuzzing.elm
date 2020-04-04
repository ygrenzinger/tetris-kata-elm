module Fuzzing exposing (..)

import Array
import Fuzz exposing (Fuzzer, constant, frequency, int, intRange, list, map)
import Random.Extra as Random
import Shape exposing (Shape, TetrominoShape, allShapes, shapeI, shapeO)
import Tetris exposing (Tetris, applyTetrominoCommand, makePieceFallDown, spawnTetromino)
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


fuzzTetrisAction : Fuzzer (Tetris -> Tetris)
fuzzTetrisAction =
    frequency
        [ ( 9, fuzzTetrominoCommand |> map applyTetrominoCommand )
        , ( 1, fuzzShape |> map (\s -> Tuple.first << spawnTetromino s [] << Tuple.first << makePieceFallDown << applyTetrominoCommand Drop) )
        ]
