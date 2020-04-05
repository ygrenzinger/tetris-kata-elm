module Grid exposing (..)

import Array exposing (Array, repeat)
import Shape exposing (ShapeColor)


type Cell
    = Empty
    | Moving ShapeColor
    | Fixed ShapeColor


type alias Row =
    Array Cell


type alias Grid =
    Array Row


createGrid : Grid
createGrid =
    Array.repeat 22 createRow


createRow : Row
createRow =
    repeat 10 Empty


updateRow : Int -> Cell -> Row -> Row
updateRow =
    Array.set


isFixedCell : Cell -> Bool
isFixedCell cell =
    case cell of
        Fixed _ ->
            True

        _ ->
            False


isEmptyCell : Cell -> Bool
isEmptyCell cell =
    case cell of
        Empty ->
            True

        _ ->
            False


isMovingCell : Cell -> Bool
isMovingCell cell =
    case cell of
        Moving _ ->
            True

        _ ->
            False


isRowFull : Row -> Bool
isRowFull =
    List.all isFixedCell << Array.toList


countCellAtState : (Cell -> Bool) -> List ( Int, Int ) -> Grid -> Int
countCellAtState fn positions grid =
    List.map
        (\pos ->
            if getCellState grid pos |> Maybe.map fn |> Maybe.withDefault False then
                1

            else
                0
        )
        positions
        |> List.sum


getCellState : Grid -> ( Int, Int ) -> Maybe Cell
getCellState grid ( i, j ) =
    Array.get i grid |> Maybe.andThen (Array.get j)


setCellState : Cell -> ( Int, Int ) -> Grid -> Grid
setCellState state ( i, j ) grid =
    let
        updatedRow =
            Array.get i grid |> Maybe.map (updateRow j state)
    in
    updatedRow |> Maybe.map (\r -> Array.set i r grid) |> Maybe.withDefault grid


projectPositions : Cell -> List ( Int, Int ) -> Grid -> Grid
projectPositions cell positions grid =
    List.foldl (setCellState cell) grid positions


cleanFullLines : Grid -> ( Grid, Int )
cleanFullLines grid =
    let
        rows =
            Array.toList grid |> List.reverse

        ( cleanedRows, numberOfRemovedLines ) =
            removeFullRows rows [] 0

        updatedGrid =
            List.append (List.repeat numberOfRemovedLines createRow) cleanedRows |> Array.fromList
    in
    ( updatedGrid, numberOfRemovedLines )


removeFullRows : List Row -> List Row -> Int -> ( List Row, Int )
removeFullRows previous new numberOfRemovedLines =
    let
        cleanRow head rest =
            if isRowFull head then
                removeFullRows rest new (numberOfRemovedLines + 1)

            else
                removeFullRows rest (head :: new) numberOfRemovedLines
    in
    case previous of
        [] ->
            ( new, numberOfRemovedLines )

        head :: [] ->
            cleanRow head []

        head :: rest ->
            cleanRow head rest
