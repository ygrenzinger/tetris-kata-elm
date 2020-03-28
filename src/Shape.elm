module Shape exposing (..)

import Array exposing (Array)

type ShapeName = I | J | L | O | S | Z | T
type ShapeCell = Empty | Full
type alias Shape = List (List ShapeCell)

isFull : ShapeCell -> Bool
isFull cell = if cell == Full then True else False

shapeSize : Shape -> Int
shapeSize = List.length

cellPositions : Shape -> List (Int,Int)
cellPositions shape =
    let
        cells = List.indexedMap (\i row -> List.indexedMap (\j cell -> (i, j, cell)) row) shape |> List.concat
        isFullPos = \(_, _, cell) -> isFull cell
    in
        cells |> List.filter isFullPos |> List.map (\(i, j, _) -> (i,j))

rotateClockWise : Shape -> Shape
rotateClockWise shape =
    let heads = List.map (List.take 1) shape |> List.concat |> List.reverse
        tails = List.map (List.drop 1) shape
    in
        if List.isEmpty heads then []
        else heads::(rotateClockWise tails)

rotateCounterClockWise : Shape -> Shape
rotateCounterClockWise shape = List.reverse (innerRotateCounterClockWise shape)

innerRotateCounterClockWise : Shape -> Shape
innerRotateCounterClockWise shape =
    let heads = List.map (List.take 1) shape |> List.concat
        tails = List.map (List.drop 1) shape
    in
        if List.isEmpty heads then []
        else (heads::(innerRotateCounterClockWise tails))

fromChar : Char -> ShapeCell
fromChar c = if c == 'X' then Full else Empty

fromStringRepresentation : List String -> Shape
fromStringRepresentation lines = List.map (String.toList >> (List.map fromChar)) lines

shapeI : Shape
shapeI = fromStringRepresentation [
        "    ",
        "XXXX",
        "    ",
        "    "
    ]

shapeJ : Shape
shapeJ = fromStringRepresentation [
                 "X  ",
                 "XXX",
                 "   "
             ]

shapeL : Shape
shapeL = fromStringRepresentation [
                 "  X",
                 "XXX",
                 "   "
             ]

shapeO : Shape
shapeO = fromStringRepresentation [
        " XX ",
        " XX ",
        "    ",
        "    "
    ]

shapeS : Shape
shapeS = fromStringRepresentation [
                 " XX",
                 "XX ",
                 "   "
             ]

shapeZ : Shape
shapeZ = fromStringRepresentation [
                 "XX ",
                 " XX",
                 "   "
             ]

shapeT : Shape
shapeT = fromStringRepresentation [
                 " X ",
                 "XXX",
                 "   "
             ]

shapes : Array Shape
shapes = Array.fromList [shapeI, shapeJ, shapeL, shapeO, shapeS, shapeZ, shapeT]
