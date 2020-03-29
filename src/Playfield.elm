module Playfield exposing (..)

import Array exposing (Array, repeat)
import Shape exposing (Shape, shapeSize)
import Tetromino exposing (Tetromino(..), TetrominoCommand(..), moveTetrominoDown, moveTetrominoLeft, moveTetrominoRight, tetrominoPositions)

type Cell = Empty | Moving | Fixed
type alias Row = Array Cell
type alias Grid = Array Row
type PlayField = PlayableField Tetromino Grid | FullField Grid

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
createPlayfield shape = spawnTetromino shape createGrid

retrieveGrid : PlayField -> Grid
retrieveGrid field = case field of
     (PlayableField _ grid) -> grid
     (FullField grid) -> grid

setGrid : PlayField -> Grid -> PlayField
setGrid field grid = case field of
     (PlayableField tetromino _) -> PlayableField tetromino grid
     (FullField _) -> FullField grid

retrieveTetromino : PlayField -> Maybe Tetromino
retrieveTetromino field = case field of
     (PlayableField tetromino _) -> Just tetromino
     (FullField _) -> Nothing

isPossiblePosition : Tetromino -> Grid -> Bool
isPossiblePosition tetromino grid = (countCellAtState Empty (tetrominoPositions tetromino) grid) == 4

spawnTetromino :  Shape -> Grid -> PlayField
spawnTetromino shape grid =
    let
        columnPos = if (shapeSize shape) == 3 then 4 else 3
        tetromino = Tetromino.Tetromino shape (0 , columnPos)
     in
        if (isPossiblePosition tetromino grid)
            then PlayableField tetromino (projectTetrominoToGrid Moving tetromino grid)
            else FullField grid

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
                    then PlayableField movedTetromino (cleanedGrid |> projectTetrominoToGrid Moving movedTetromino)
                    else PlayableField tetromino grid

applyCommand : TetrominoCommand -> PlayField -> PlayField
applyCommand command field =
    case field of
        FullField _ -> field
        PlayableField tetromino grid -> moveTetrominoOnGrid command tetromino grid

fixTetromino : PlayField -> PlayField
fixTetromino field =
    case field of
        FullField _ -> field
        PlayableField tetromino grid -> PlayableField tetromino (projectTetrominoToGrid Fixed tetromino grid)

