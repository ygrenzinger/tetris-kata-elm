module Fuzzing exposing (..)

import Array
import Fuzz exposing (Fuzzer, int, intRange, map)
import Shape exposing (Shape, allShapes, shapeI)
import Tetromino exposing (MoveCommand(..), TetrominoCommand(..))


retrieveShape : Int -> Shape
retrieveShape i =
    let
        shapesArray =
            Array.fromList allShapes
    in
    Array.get (modBy (Array.length shapesArray) i) shapesArray |> Maybe.withDefault shapeI


fuzzShape : Fuzzer Shape
fuzzShape =
    int |> map retrieveShape


chooseCommand : Int -> TetrominoCommand
chooseCommand i =
    case i of
        0 ->
            Move MoveLeft

        1 ->
            Move MoveRight

        _ ->
            Move MoveDown


fuzzMoveCommand : Fuzzer TetrominoCommand
fuzzMoveCommand =
    intRange 0 2 |> map chooseCommand
