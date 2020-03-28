module Playfield exposing (..)

import Array exposing (Array, repeat)
import Shape exposing (Shape, cellPositions, shapeSize)
import Tetromino exposing (Tetromino(..), TetrominoCommand(..), moveTetrominoDown, moveTetrominoLeft, moveTetrominoRight)

type Cell = Empty | Moving
type alias Row = Array Cell
type alias Grid = Array Row
type PlayField = PlayableField Tetromino Grid | FullField Grid

createRow : Row
createRow = (repeat 10 Empty)

updateRow : Int -> Cell -> Row -> Row
updateRow = Array.set

countCellAtState : Cell -> List (Int, Int) -> Grid -> Int
countCellAtState state positions grid =
    List.map (\pos -> if ((getCellState pos grid) == (Just state)) then 1 else 0) positions
        |> List.sum

getCellState : (Int, Int) -> Grid -> Maybe Cell
getCellState (i,j) grid = Array.get i grid |> Maybe.andThen (Array.get j)

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

spawnTetromino :  Shape -> Grid -> PlayField
spawnTetromino shape grid =
    let
        columnPos = if (shapeSize shape) == 3 then 4 else 3
        tetromino = (Tetromino.Tetromino shape (0 , columnPos))
        updatedGrid = placeMovingTetromino tetromino grid
     in
        updatedGrid |> Maybe.map (PlayableField tetromino) |> Maybe.withDefault (FullField grid)

placeMovingTetromino : Tetromino -> Grid -> Maybe Grid
placeMovingTetromino (Tetromino shape (i,j)) grid =
    let
        movingPositions = List.map (\(ii,jj) -> (ii + i, jj + j))  (cellPositions shape)
        updatedGrid = List.foldl (setCellState Moving) grid movingPositions
    in
        Just updatedGrid

moveTetrominoOnGrid : TetrominoCommand -> Tetromino -> Grid -> PlayField
moveTetrominoOnGrid command tetromino grid =
    let
        movedTetromino = case command of
            MoveDown -> moveTetrominoDown tetromino
            MoveLeft -> moveTetrominoLeft tetromino
            MoveRight -> moveTetrominoRight tetromino
        updatedGrid =  placeMovingTetromino movedTetromino grid
    in
        updatedGrid
            |> Maybe.map (PlayableField movedTetromino)
            |> Maybe.withDefault  (PlayableField tetromino grid)

applyCommand : TetrominoCommand -> PlayField -> PlayField
applyCommand command field =
    case field of
        FullField _ -> field
        PlayableField tetromino grid -> moveTetrominoOnGrid command tetromino grid

