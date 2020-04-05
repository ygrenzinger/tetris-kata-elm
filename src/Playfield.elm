module Playfield exposing (..)

import Array exposing (Array)
import Grid exposing (Cell(..), Grid, Row, cleanFullLines, countCellAtState, createRow, isEmptyCell, isMovingCell, projectPositions)
import Shape exposing (Shape, ShapeColor, TetrominoShape, shapeSize)
import Tetromino as T exposing (MoveCommand(..), Tetromino(..), TetrominoCommand(..))


type PlayField
    = PlayField (Maybe Tetromino) Grid


type PlayFieldState
    = Playable
    | Full


createGrid : Grid
createGrid =
    Array.repeat 22 createRow


createPlayfield : PlayField
createPlayfield =
    PlayField Nothing createGrid


retrieveGrid : PlayField -> Grid
retrieveGrid (PlayField _ grid) =
    grid


isPossiblePosition : Tetromino -> PlayField -> Bool
isPossiblePosition tetromino field =
    countCellAtState (\c -> isEmptyCell c || isMovingCell c) (T.positions tetromino) (retrieveGrid field) == 4


spawnTetromino : TetrominoShape -> PlayField -> ( PlayField, PlayFieldState )
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


applyCommand : TetrominoCommand -> PlayField -> PlayField
applyCommand command playfield =
    case playfield of
        PlayField (Just tetromino) _ ->
            applyCommandOnTetromino command tetromino playfield

        (PlayField Nothing _) as p ->
            p


applyCommandOnTetromino : TetrominoCommand -> Tetromino -> PlayField -> PlayField
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


tryWallKick : T.WallKick -> T.RotateCommand -> Tetromino -> PlayField -> PlayField
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


makeTetrominoFallDown : PlayField -> ( PlayField, Maybe Int )
makeTetrominoFallDown (PlayField tetromino grid) =
    case tetromino of
        Nothing ->
            ( PlayField tetromino grid, Nothing )

        Just t ->
            tetrominoFallDown t (PlayField tetromino grid)


tetrominoFallDown : Tetromino -> PlayField -> ( PlayField, Maybe Int )
tetrominoFallDown tetromino playfield =
    let
        (PlayField updatedTetromino updatedGrid) =
            applyCommand (Move MoveDown) playfield
    in
    if Maybe.map (T.samePosition tetromino) updatedTetromino |> Maybe.withDefault False then
        fixTetromino playfield
            |> cleanFullLinesAndRemoveTetromino
            |> Tuple.mapSecond Just

    else
        ( PlayField updatedTetromino updatedGrid, Nothing )


removeTetromino : PlayField -> PlayField
removeTetromino (PlayField tetromino grid) =
    case tetromino of
        Nothing ->
            PlayField Nothing grid

        Just t ->
            PlayField Nothing <| projectPositions Empty (T.positions t) grid


projectMovingTetromino : Tetromino -> PlayField -> PlayField
projectMovingTetromino tetromino (PlayField _ grid) =
    PlayField (Just tetromino) <| projectPositions (Moving (T.getColor tetromino)) (T.positions tetromino) grid


fixTetromino : PlayField -> PlayField
fixTetromino (PlayField tetromino grid) =
    case tetromino of
        Nothing ->
            PlayField Nothing grid

        Just t ->
            PlayField Nothing <| projectPositions (Fixed (T.getColor t)) (T.positions t) grid


updateTetrominoPosition : Tetromino -> PlayField -> Maybe PlayField
updateTetrominoPosition updatedTetromino field =
    if isPossiblePosition updatedTetromino field then
        Just <| projectMovingTetromino updatedTetromino <| removeTetromino field

    else
        Nothing


cleanFullLinesAndRemoveTetromino : PlayField -> ( PlayField, Int )
cleanFullLinesAndRemoveTetromino (PlayField _ grid) =
    cleanFullLines grid |> Tuple.mapFirst (PlayField Nothing)
