module Playfield exposing (..)

import Array exposing (Array)
import Grid exposing (Cell(..), Grid, Row, cleanFullLines, countCellAtState, createRow, isEmptyCell, isMovingCell, projectPositions)
import Shape exposing (Shape, ShapeColor, TetrominoShape, shapeSize)
import Tetromino as T exposing (MoveCommand(..), Tetromino(..), TetrominoCommand(..))


type Playfield
    = Playfield (Maybe Tetromino) Grid


type PlayfieldState
    = Playable
    | Full


createGrid : Grid
createGrid =
    Array.repeat 22 createRow


createPlayfield : Playfield
createPlayfield =
    Playfield Nothing createGrid


retrieveGrid : Playfield -> Grid
retrieveGrid (Playfield _ grid) =
    grid


isPossiblePosition : Tetromino -> Playfield -> Bool
isPossiblePosition tetromino field =
    countCellAtState (\c -> isEmptyCell c || isMovingCell c) (T.positions tetromino) (retrieveGrid field) == 4


spawnTetromino : TetrominoShape -> Playfield -> ( Playfield, PlayfieldState )
spawnTetromino shape field =
    let
        columnPos =
            if shapeSize shape == 3 then
                4

            else
                3

        tetromino =
            Tetromino shape ( 0, columnPos )
    in
    if isPossiblePosition tetromino field then
        ( projectMovingTetromino tetromino field, Playable )

    else
        ( fixTetromino field, Full )


applyCommand : TetrominoCommand -> Playfield -> Playfield
applyCommand command playfield =
    case playfield of
        Playfield (Just tetromino) _ ->
            applyCommandOnTetromino command tetromino playfield

        (Playfield Nothing _) as p ->
            p


applyCommandOnTetromino : TetrominoCommand -> Tetromino -> Playfield -> Playfield
applyCommandOnTetromino command tetromino playfield =
    let
        updatedTetromino =
            T.applyCommand command tetromino
    in
    case ( command, updateTetrominoPosition updatedTetromino playfield ) of
        ( Drop, Just updatedField ) ->
            applyCommandOnTetromino Drop updatedTetromino updatedField

        ( T.Rotate _, Just updatedField ) ->
            updatedField

        ( T.Rotate (_ as rotateCommand), Nothing ) ->
            case T.whichWallKickToAttempt tetromino of
                Nothing ->
                    playfield

                Just wallKick ->
                    tryWallKick wallKick rotateCommand tetromino playfield

        ( T.Move _, Just updatedField ) ->
            updatedField

        _ ->
            playfield


tryWallKick : T.WallKick -> T.RotateCommand -> Tetromino -> Playfield -> Playfield
tryWallKick wallKick command tetromino playfield =
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
    case updateTetrominoPosition updatedTetromino playfield of
        Just updatedField ->
            updatedField

        Nothing ->
            playfield


makeTetrominoFallDown : Playfield -> ( Playfield, Maybe Int )
makeTetrominoFallDown (Playfield tetromino grid) =
    case tetromino of
        Nothing ->
            ( Playfield tetromino grid, Nothing )

        Just t ->
            tetrominoFallDown t (Playfield tetromino grid)


tetrominoFallDown : Tetromino -> Playfield -> ( Playfield, Maybe Int )
tetrominoFallDown tetromino playfield =
    let
        (Playfield updatedTetromino updatedGrid) =
            applyCommand (Move MoveDown) playfield
    in
    if Maybe.map (T.samePosition tetromino) updatedTetromino |> Maybe.withDefault False then
        fixTetromino playfield
            |> cleanFullLinesAndRemoveTetromino
            |> Tuple.mapSecond Just

    else
        ( Playfield updatedTetromino updatedGrid, Nothing )


removeTetromino : Playfield -> Playfield
removeTetromino (Playfield tetromino grid) =
    case tetromino of
        Nothing ->
            Playfield Nothing grid

        Just t ->
            Playfield Nothing <| projectPositions Empty (T.positions t) grid


projectMovingTetromino : Tetromino -> Playfield -> Playfield
projectMovingTetromino tetromino (Playfield _ grid) =
    Playfield (Just tetromino) <| projectPositions (Moving (T.getColor tetromino)) (T.positions tetromino) grid


fixTetromino : Playfield -> Playfield
fixTetromino (Playfield tetromino grid) =
    case tetromino of
        Nothing ->
            Playfield Nothing grid

        Just t ->
            Playfield Nothing <| projectPositions (Fixed (T.getColor t)) (T.positions t) grid


updateTetrominoPosition : Tetromino -> Playfield -> Maybe Playfield
updateTetrominoPosition updatedTetromino field =
    if isPossiblePosition updatedTetromino field then
        Just <| projectMovingTetromino updatedTetromino <| removeTetromino field

    else
        Nothing


cleanFullLinesAndRemoveTetromino : Playfield -> ( Playfield, Int )
cleanFullLinesAndRemoveTetromino (Playfield _ grid) =
    cleanFullLines grid |> Tuple.mapFirst (Playfield Nothing)
