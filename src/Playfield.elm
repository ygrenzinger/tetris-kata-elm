module Playfield exposing (..)

import Array exposing (Array, repeat)
import Shape exposing (Shape, shapeSize)
import Tetromino as T exposing (Tetromino(..), TetrominoCommand(..))

type Cell = Empty | Moving | Fixed
type alias Row = Array Cell
type alias Grid = Array Row
type PlayField = PlayField Tetromino Grid
type PlayFieldState = Playable | Full

createRow : Row
createRow = (repeat 10 Empty)

updateRow : Int -> Cell -> Row -> Row
updateRow = Array.set

isRowFullOf : Cell -> Row -> Bool
isRowFullOf cell = List.all (\c -> c == cell) << Array.toList

countCellAtState : Cell -> List (Int, Int) -> Grid -> Int
countCellAtState state positions grid =
    List.map (\pos -> if ((getCellState grid pos) == (Just state)) then 1 else 0) positions
        |> List.sum

getCellState : Grid -> (Int, Int) -> Maybe Cell
getCellState grid (i,j) = Array.get i grid |> Maybe.andThen (Array.get j)

setCellState : Cell -> (Int, Int) -> Grid -> Grid
setCellState state (i,j) grid =
    let
        updatedRow = (Array.get i grid) |> Maybe.map (updateRow j state)
    in
        updatedRow |> Maybe.map (\r -> Array.set i r grid) |> Maybe.withDefault grid

createGrid : Grid
createGrid = Array.repeat 20 createRow

createPlayfield : Shape -> PlayField
createPlayfield shape = case (spawnTetromino shape createGrid) of
    (_ as field, _) -> field

retrieveGrid : PlayField -> Grid
retrieveGrid (PlayField _ grid) = grid

setGrid : PlayField -> Grid -> PlayField
setGrid (PlayField tetromino _) grid = PlayField tetromino grid

retrieveTetromino : PlayField -> Tetromino
retrieveTetromino (PlayField tetromino _) = tetromino

isPossiblePosition : Tetromino -> Grid -> Bool
isPossiblePosition tetromino grid =
    (countCellAtState Empty (T.positions tetromino) grid) == 4

spawnTetromino :  Shape -> Grid -> (PlayField, PlayFieldState)
spawnTetromino shape grid =
    let
        columnPos = if (shapeSize shape) == 3 then 4 else 3
        tetromino = Tetromino shape (0 , columnPos)
        field = PlayField tetromino <| projectTetrominoToGrid Moving tetromino grid
     in
        if (isPossiblePosition tetromino grid)
            then (field, Playable)
            else (field, Full)

projectTetrominoToGrid : Cell -> Tetromino -> Grid -> Grid
projectTetrominoToGrid cell tetromino grid =
    List.foldl (setCellState cell) grid (T.positions tetromino)

tryWallKick : T.WallKick -> T.RotateCommand -> Tetromino -> Grid -> PlayField
tryWallKick wallKick command tetromino grid =
    let
        moveFn : Tetromino -> Tetromino
        moveFn = case wallKick of
            T.LeftWallKick i -> List.foldl (\f g -> f << g) T.moveTetrominoRight (List.repeat (i - 1) T.moveTetrominoRight)
            T.RightWallKick i -> List.foldl (\f g -> f << g) T.moveTetrominoLeft (List.repeat (i - 1) T.moveTetrominoLeft)
        updatedTetromino = moveFn tetromino |> case command of
            T.RotateLeft -> T.rotateTetrominoLeft
            T.RotateRight -> T.rotateTetrominoRight
        cleanedGrid = projectTetrominoToGrid Empty tetromino grid
        isPossible = isPossiblePosition updatedTetromino cleanedGrid
    in if isPossible
        then PlayField updatedTetromino (cleanedGrid |> projectTetrominoToGrid Moving updatedTetromino)
        else PlayField tetromino grid

applyCommand : TetrominoCommand -> PlayField -> PlayField
applyCommand command (PlayField tetromino grid) =
    let
        updatedTetromino = case command of
            (T.Move T.MoveDown) -> T.moveTetrominoDown tetromino
            (T.Move T.MoveLeft) -> T.moveTetrominoLeft tetromino
            (T.Move T.MoveRight) -> T.moveTetrominoRight tetromino
            (T.Rotate T.RotateLeft) -> T.rotateTetrominoLeft tetromino
            (T.Rotate T.RotateRight) -> T.rotateTetrominoRight tetromino
        cleanedGrid = projectTetrominoToGrid Empty tetromino grid
        isPossible = isPossiblePosition updatedTetromino cleanedGrid
    in
        case (command, isPossible) of
            ((T.Rotate _ ), True) -> PlayField updatedTetromino (cleanedGrid |> projectTetrominoToGrid Moving updatedTetromino)
            ((T.Rotate (_ as rotateCommand)) , False) ->
                case (T.whichWallKickToAttempt tetromino) of
                    Nothing -> PlayField tetromino grid
                    Just wallKick -> tryWallKick wallKick rotateCommand tetromino grid
            (_, True) -> PlayField updatedTetromino (cleanedGrid |> projectTetrominoToGrid Moving updatedTetromino)
            (_, False) -> PlayField tetromino grid

fixTetromino : PlayField -> PlayField
fixTetromino (PlayField tetromino grid) =
    PlayField tetromino  <| projectTetrominoToGrid Fixed tetromino grid

removeFullRows : List Row -> List Row -> Int -> (List Row, Int)
removeFullRows previous new numberOfRemovedLines =
    let
        cleanRow head rest = if (isRowFullOf Fixed head)
                            then removeFullRows rest (new) (numberOfRemovedLines + 1)
                            else removeFullRows rest (head::new) numberOfRemovedLines
    in
        case previous of
            [] -> (new, numberOfRemovedLines)
            head::[] -> cleanRow head []
            head::rest -> cleanRow head rest

cleanFullLines : PlayField -> (PlayField, Int)
cleanFullLines (PlayField tetromino grid) =
    let
        rows = Array.toList grid |> List.reverse
        (cleanedRows, numberOfRemovedLines) = removeFullRows rows [] 0
        updatedGrid = List.append (List.repeat numberOfRemovedLines createRow) cleanedRows |> Array.fromList
    in
        (PlayField tetromino updatedGrid, numberOfRemovedLines)

pieceFallingDown : PlayField -> (PlayField, Maybe Int)
pieceFallingDown (PlayField tetromino grid) =
    let
        updatedTetromino = T.moveTetrominoDown tetromino
    in
        if T.samePosition updatedTetromino tetromino
            then fixPieceFallingDown (PlayField tetromino grid)
            else ((PlayField updatedTetromino grid), Nothing)

fixPieceFallingDown : PlayField -> (PlayField, Maybe Int)
fixPieceFallingDown field = fixTetromino field |> cleanFullLines |> Tuple.mapSecond Just