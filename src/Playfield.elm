module Playfield exposing (..)

import Array exposing (Array)
import Grid exposing (Cell(..), Grid, Row, cleanFullLines, countCellAtState, createRow, fixTetromino, isEmptyCell, isMovingCell, projectMovingTetromino, removeTetromino)
import Shape exposing (Shape, ShapeColor, TetrominoShape, shapeSize)
import Tetromino as T exposing (MoveCommand(..), Tetromino(..), TetrominoCommand(..))


type Playfield
    = WaitingForTetromino Grid
    | Playable Tetromino Grid
    | Full Grid


createGrid : Grid
createGrid =
    Array.repeat 22 createRow


createPlayfield : Playfield
createPlayfield =
    WaitingForTetromino createGrid


retrieveGrid : Playfield -> Grid
retrieveGrid field =
    case field of
        WaitingForTetromino grid ->
            grid

        Playable _ grid ->
            grid

        Full grid ->
            grid


isFull : Playfield -> Bool
isFull field =
    case field of
        Full _ ->
            True

        _ ->
            False


isPossiblePosition : Tetromino -> Grid -> Bool
isPossiblePosition tetromino grid =
    countCellAtState (\c -> isEmptyCell c || isMovingCell c) (T.positions tetromino) grid == 4


initTetromino : TetrominoShape -> Tetromino
initTetromino shape =
    let
        columnPos =
            if shapeSize shape == 3 then
                4

            else
                3
    in
    Tetromino shape ( 0, columnPos )


spawnTetromino : TetrominoShape -> Playfield -> Playfield
spawnTetromino shape field =
    case field of
        WaitingForTetromino grid ->
            spawnTetrominoOnGrid (initTetromino shape) grid

        Playable _ _ ->
            field

        Full _ ->
            field


spawnTetrominoOnGrid : Tetromino -> Grid -> Playfield
spawnTetrominoOnGrid tetromino grid =
    if isPossiblePosition tetromino grid then
        Playable tetromino (projectMovingTetromino tetromino grid)

    else
        Full grid


applyCommand : TetrominoCommand -> Playfield -> Playfield
applyCommand command field =
    case field of
        Playable tetromino grid ->
            applyCommandOnTetromino command tetromino grid

        WaitingForTetromino _ ->
            field

        Full _ ->
            field


applyCommandOnTetromino : TetrominoCommand -> Tetromino -> Grid -> Playfield
applyCommandOnTetromino command tetromino grid =
    let
        updatedTetromino =
            T.applyCommand command tetromino

        possibleGrid =
            updateTetrominoPosition updatedTetromino <| removeTetromino tetromino grid
    in
    case ( command, possibleGrid ) of
        ( Drop, Just updatedGrid ) ->
            applyCommand Drop (Playable updatedTetromino updatedGrid)

        ( T.Rotate _, Just updatedGrid ) ->
            Playable updatedTetromino updatedGrid

        ( T.Rotate (_ as rotateCommand), Nothing ) ->
            case T.whichWallKickToAttempt tetromino of
                Nothing ->
                    Playable tetromino grid

                Just wallKick ->
                    tryWallKick wallKick rotateCommand tetromino grid

        ( T.Move _, Just updatedGrid ) ->
            Playable updatedTetromino updatedGrid

        _ ->
            Playable tetromino grid


tryWallKick : T.WallKick -> T.RotateCommand -> Tetromino -> Grid -> Playfield
tryWallKick wallKick command tetromino grid =
    let
        doWallKickFn : Tetromino -> Tetromino
        doWallKickFn =
            case wallKick of
                T.LeftWallKick i ->
                    List.foldl (\f g -> f << g) T.moveTetrominoRight (List.repeat (i - 1) T.moveTetrominoRight)

                T.RightWallKick i ->
                    List.foldl (\f g -> f << g) T.moveTetrominoLeft (List.repeat (i - 1) T.moveTetrominoLeft)

        updatedTetromino =
            doWallKickFn tetromino
                |> (case command of
                        T.RotateLeft ->
                            T.rotateTetrominoLeft

                        T.RotateRight ->
                            T.rotateTetrominoRight
                   )
    in
    case updateTetrominoPosition updatedTetromino grid of
        Just updatedGrid ->
            Playable updatedTetromino updatedGrid

        Nothing ->
            Playable tetromino grid


makeTetrominoFallDown : Playfield -> ( Playfield, Maybe Int )
makeTetrominoFallDown field =
    case field of
        Playable tetromino grid ->
            tetrominoFallDown tetromino grid

        _ ->
            ( field, Nothing )


tetrominoFallDown : Tetromino -> Grid -> ( Playfield, Maybe Int )
tetrominoFallDown tetromino grid =
    let
        updatedTetromino =
            T.applyCommand (Move MoveDown) tetromino

        possibleGrid =
            updateTetrominoPosition updatedTetromino <| removeTetromino tetromino grid
    in
    case possibleGrid of
        Just updatedGrid ->
            ( Playable updatedTetromino updatedGrid, Nothing )

        Nothing ->
            fixTetromino tetromino grid
                |> cleanFullLines
                |> Tuple.mapFirst WaitingForTetromino
                |> Tuple.mapSecond Just


updateTetrominoPosition : Tetromino -> Grid -> Maybe Grid
updateTetrominoPosition tetromino grid =
    if isPossiblePosition tetromino grid then
        Just <| projectMovingTetromino tetromino grid

    else
        Nothing
