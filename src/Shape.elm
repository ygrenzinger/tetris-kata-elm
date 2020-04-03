module Shape exposing (..)

import Random
import Random.List


type ShapeName
    = I
    | J
    | L
    | O
    | S
    | Z
    | T


type ShapeCell
    = Empty
    | Full


type alias Shape =
    List (List ShapeCell)


type alias ShapeColor =
    String


type TetrominoShape
    = TetrominoShape ShapeName ShapeColor Shape


getColor : TetrominoShape -> ShapeColor
getColor (TetrominoShape _ color _) =
    color


isFull : ShapeCell -> Bool
isFull cell =
    if cell == Full then
        True

    else
        False


shapeSize : TetrominoShape -> Int
shapeSize (TetrominoShape _ _ shape) =
    List.length shape


cellPositions : TetrominoShape -> List ( Int, Int )
cellPositions (TetrominoShape _ _ shape) =
    let
        cells =
            List.indexedMap (\i row -> List.indexedMap (\j cell -> ( i, j, cell )) row) shape |> List.concat

        isFullPos =
            \( _, _, cell ) -> isFull cell
    in
    cells |> List.filter isFullPos |> List.map (\( i, j, _ ) -> ( i, j ))


rotateClockWise : TetrominoShape -> TetrominoShape
rotateClockWise (TetrominoShape name color shape) =
    TetrominoShape name color (rotateShapeClockWise shape)


rotateShapeClockWise : Shape -> Shape
rotateShapeClockWise shape =
    let
        heads =
            List.map (List.take 1) shape |> List.concat |> List.reverse

        tails =
            List.map (List.drop 1) shape
    in
    if List.isEmpty heads then
        []

    else
        heads :: rotateShapeClockWise tails


rotateCounterClockWise : TetrominoShape -> TetrominoShape
rotateCounterClockWise (TetrominoShape name color shape) =
    TetrominoShape name color (rotateShapeCounterClockWise shape)


rotateShapeCounterClockWise : Shape -> Shape
rotateShapeCounterClockWise shape =
    List.reverse (innerShapeRotateCounterClockWise shape)


innerShapeRotateCounterClockWise : Shape -> Shape
innerShapeRotateCounterClockWise shape =
    let
        heads =
            List.map (List.take 1) shape |> List.concat

        tails =
            List.map (List.drop 1) shape
    in
    if List.isEmpty heads then
        []

    else
        heads :: innerShapeRotateCounterClockWise tails


fromChar : Char -> ShapeCell
fromChar c =
    if c == 'X' then
        Full

    else
        Empty


fromStringRepresentation : List String -> Shape
fromStringRepresentation lines =
    List.map (String.toList >> List.map fromChar) lines


shapeI : TetrominoShape
shapeI =
    TetrominoShape I "00FFFF" <|
        fromStringRepresentation
            [ "    "
            , "XXXX"
            , "    "
            , "    "
            ]


shapeJ : TetrominoShape
shapeJ =
    TetrominoShape J "00008B" <|
        fromStringRepresentation
            [ "X  "
            , "XXX"
            , "   "
            ]


shapeL : TetrominoShape
shapeL =
    TetrominoShape L "FF8C00" <|
        fromStringRepresentation
            [ "  X"
            , "XXX"
            , "   "
            ]


shapeO : TetrominoShape
shapeO =
    TetrominoShape O "FFD700" <|
        fromStringRepresentation
            [ " XX "
            , " XX "
            , "    "
            , "    "
            ]


shapeS : TetrominoShape
shapeS =
    TetrominoShape S "008000" <|
        fromStringRepresentation
            [ " XX"
            , "XX "
            , "   "
            ]


shapeZ : TetrominoShape
shapeZ =
    TetrominoShape Z "FF0000" <|
        fromStringRepresentation
            [ "XX "
            , " XX"
            , "   "
            ]


shapeT : TetrominoShape
shapeT =
    TetrominoShape T "800080" <|
        fromStringRepresentation
            [ " X "
            , "XXX"
            , "   "
            ]


allShapes : List TetrominoShape
allShapes =
    [ shapeI, shapeJ, shapeL, shapeO, shapeS, shapeZ, shapeT ]


randomShapeGenerator : List TetrominoShape -> Random.Generator ( Maybe TetrominoShape, List TetrominoShape )
randomShapeGenerator possibleShapes =
    Random.List.choose possibleShapes
