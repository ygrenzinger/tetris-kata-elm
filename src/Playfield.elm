module Playfield exposing (..)

import Array exposing (Array)
import Grid exposing (Cell(..), Grid, Row, cleanFullLines, countCellAtState, createRow, isEmptyCell, projectPositions)
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
isPossiblePosition tetromino grid =
    countCellAtState isEmptyCell (T.positions tetromino) (retrieveGrid grid) == 4


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
        ( projectTetrominoToGrid (Moving (T.getColor tetromino)) tetromino field, Playable )

    else
        ( projectTetrominoToGrid (Fixed (T.getColor tetromino)) tetromino field, Full )


projectTetrominoToGrid : Cell -> Tetromino -> PlayField -> PlayField
projectTetrominoToGrid cell tetromino (PlayField _ grid) =
    PlayField (Just tetromino) <| projectPositions cell (T.positions tetromino) grid


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

        cleanedField =
            projectTetrominoToGrid Empty tetromino playfield

        isPossible =
            isPossiblePosition updatedTetromino cleanedField
    in
    case ( command, isPossible ) of
        ( Drop, True ) ->
            applyCommandOnTetromino Drop updatedTetromino cleanedField

        ( Drop, False ) ->
            projectTetrominoToGrid (Moving (T.getColor updatedTetromino)) tetromino cleanedField

        ( T.Rotate _, True ) ->
            projectTetrominoToGrid (Moving (T.getColor updatedTetromino)) updatedTetromino cleanedField

        ( T.Rotate (_ as rotateCommand), False ) ->
            case T.whichWallKickToAttempt tetromino of
                Nothing ->
                    playfield

                Just wallKick ->
                    tryWallKick wallKick rotateCommand tetromino playfield

        ( T.Move _, True ) ->
            projectTetrominoToGrid (Moving (T.getColor updatedTetromino)) updatedTetromino cleanedField

        _ ->
            playfield


dropTetromino : Tetromino -> PlayField -> PlayField
dropTetromino tetromino playfield =
    let
        updatedTetromino =
            T.applyCommand Drop tetromino

        cleanedField =
            projectTetrominoToGrid Empty tetromino playfield

        isPossible =
            isPossiblePosition updatedTetromino cleanedField
    in
    if isPossible then
        dropTetromino updatedTetromino playfield

    else
        projectTetrominoToGrid (Moving (T.getColor updatedTetromino)) updatedTetromino cleanedField


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

        cleanedField =
            projectTetrominoToGrid Empty tetromino playfield

        isPossible =
            isPossiblePosition updatedTetromino cleanedField
    in
    if isPossible then
        projectTetrominoToGrid (Moving (T.getColor updatedTetromino)) updatedTetromino cleanedField

    else
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
        projectTetrominoToGrid (Fixed (T.getColor tetromino)) tetromino playfield
            |> cleanFullLinesAndRemoveTetromino
            |> Tuple.mapSecond Just

    else
        ( PlayField updatedTetromino updatedGrid, Nothing )


cleanFullLinesAndRemoveTetromino : PlayField -> ( PlayField, Int )
cleanFullLinesAndRemoveTetromino (PlayField _ grid) =
    cleanFullLines grid |> Tuple.mapFirst (PlayField Nothing)
