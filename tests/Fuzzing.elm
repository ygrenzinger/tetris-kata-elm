module Fuzzing exposing (..)

import Array
import Fuzz exposing (Fuzzer, int, map)
import Shape exposing (Shape, shapeI, shapes)

retrieveShape : Int -> Shape
retrieveShape i = Array.get (modBy (Array.length shapes) i) shapes |> Maybe.withDefault shapeI

fuzzShape : Fuzzer Shape
fuzzShape = int |> map retrieveShape