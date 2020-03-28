module Fuzzing exposing (..)

import Array
import Fuzz exposing (Fuzzer, int, intRange, map)
import Shape exposing (Shape, shapeI, shapes)
import Tetromino exposing (TetrominoCommand(..))

retrieveShape : Int -> Shape
retrieveShape i = Array.get (modBy (Array.length shapes) i) shapes |> Maybe.withDefault shapeI

fuzzShape : Fuzzer Shape
fuzzShape = int |> map retrieveShape

chooseCommand : Int -> TetrominoCommand
chooseCommand i = case i of
    0 -> MoveLeft
    1 -> MoveRight
    _ -> MoveDown

fuzzCommand : Fuzzer TetrominoCommand
fuzzCommand = intRange 0 2 |> map chooseCommand