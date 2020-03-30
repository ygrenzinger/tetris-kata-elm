module Playfield exposing (..)

import Array exposing (Array, repeat)
import Shape exposing (Shape, shapeSize)
import Tetromino exposing (MoveCommand(..), RotateCommand(..), Tetromino(..), TetrominoCommand(..), WallKick(..), moveTetrominoDown, moveTetrominoLeft, moveTetrominoRight, rotateTetrominoLeft, rotateTetrominoRight, tetrominoPositions, whichWallKickToAttempt)

type Cell = Empty | Moving | Fixed
type alias Row = Array Cell
type alias Grid = Array Row
type PlayField = PlayField Tetromino Grid
type PlayFieldState = Playable | Full

createRow : Row
createRow = (repeat 10 Empty)

updateRow : Int -> Cell -> Row -> Row
updateRow = Array.set

isRowAtState : Cell -> Row -> Bool
isRowAtState cell = List.all (\c -> c == cell) << Array.toList

isFullRow : Row -> Bool
isFullRow = isRowAtState Fixed

isEmptyRow : Row -> Bool
isEmptyRow = isRowAtState Empty

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
    (countCellAtState Empty (tetrominoPositions tetromino) grid) == 4

spawnTetromino :  Shape -> Grid -> (PlayField, PlayFieldState)
spawnTetromino shape grid =
    let
        columnPos = if (shapeSize shape) == 3 then 4 else 3
        tetromino = Tetromino.Tetromino shape (0 , columnPos)
        field = PlayField tetromino <| projectTetrominoToGrid Moving tetromino grid
     in
        if (isPossiblePosition tetromino grid)
            then (field, Playable)
            else (field, Full)

projectTetrominoToGrid : Cell -> Tetromino -> Grid -> Grid
projectTetrominoToGrid cell tetromino grid =
    List.foldl (setCellState cell) grid (tetrominoPositions tetromino)

tryWallKick : WallKick -> RotateCommand -> Tetromino -> Grid -> PlayField
tryWallKick wallKick command tetromino grid =
    let
        moveFn : Tetromino -> Tetromino
        moveFn = case wallKick of
            LeftWallKick i -> List.foldl (\f g -> f << g) moveTetrominoRight (List.repeat (i - 1) moveTetrominoRight)
            RightWallKick i -> List.foldl (\f g -> f << g) moveTetrominoLeft (List.repeat (i - 1) moveTetrominoLeft)
        updatedTetromino = moveFn tetromino |> case command of
            RotateLeft -> rotateTetrominoLeft
            RotateRight -> rotateTetrominoRight
        cleanedGrid = projectTetrominoToGrid Empty tetromino grid
        isPossible = isPossiblePosition updatedTetromino cleanedGrid
    in if isPossible
        then PlayField updatedTetromino (cleanedGrid |> projectTetrominoToGrid Moving updatedTetromino)
        else PlayField tetromino grid

applyCommand : TetrominoCommand -> PlayField -> PlayField
applyCommand command (PlayField tetromino grid) =
    let
        updatedTetromino = case command of
            (Move MoveDown) -> moveTetrominoDown tetromino
            (Move MoveLeft) -> moveTetrominoLeft tetromino
            (Move MoveRight) -> moveTetrominoRight tetromino
            (Rotate RotateLeft) -> rotateTetrominoLeft tetromino
            (Rotate RotateRight) -> rotateTetrominoRight tetromino
        cleanedGrid = projectTetrominoToGrid Empty tetromino grid
        isPossible = isPossiblePosition updatedTetromino cleanedGrid
    in
        case (command, isPossible) of
            ((Rotate _ ), True) -> PlayField updatedTetromino (cleanedGrid |> projectTetrominoToGrid Moving updatedTetromino)
            ((Rotate (_ as rotateCommand)) , False) ->
                case (whichWallKickToAttempt tetromino) of
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
        cleanRow head rest = if (isFullRow head)
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


