module Playfield exposing (..)

import Array exposing (Array, repeat)
import Shape exposing (Shape, shapeSize)
import Tetromino exposing (Tetromino(..), TetrominoCommand(..), moveTetrominoDown, moveTetrominoLeft, moveTetrominoRight, tetrominoPositions)

type Cell = Empty | Moving | Fixed
type alias Row = Array Cell
type alias Grid = Array Row
type PlayField = PlayField Tetromino Grid
type PlayFieldState = Playable | Full

createRow : Row
createRow = (repeat 10 Empty)

updateRow : Int -> Cell -> Row -> Row
updateRow = Array.set

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

moveTetrominoOnGrid : TetrominoCommand -> Tetromino -> Grid -> PlayField
moveTetrominoOnGrid command tetromino grid =
    let
        movedTetromino = case command of
            MoveDown -> moveTetrominoDown tetromino
            MoveLeft -> moveTetrominoLeft tetromino
            MoveRight -> moveTetrominoRight tetromino
        cleanedGrid = projectTetrominoToGrid Empty tetromino grid
    in
        if (isPossiblePosition movedTetromino cleanedGrid)
                    then PlayField movedTetromino (cleanedGrid |> projectTetrominoToGrid Moving movedTetromino)
                    else PlayField tetromino grid

applyCommand : TetrominoCommand -> PlayField -> PlayField
applyCommand command (PlayField tetromino grid) =
    moveTetrominoOnGrid command tetromino grid

fixTetromino : PlayField -> PlayField
fixTetromino (PlayField tetromino grid) =
    PlayField tetromino  <| projectTetrominoToGrid Fixed tetromino grid

