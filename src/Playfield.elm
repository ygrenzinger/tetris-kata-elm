module Playfield exposing (..)

import Array exposing (Array, repeat)
import Shape exposing (Shape, ShapeColor, TetrominoShape, shapeSize)
import Tetromino as T exposing (MoveCommand(..), Tetromino(..), TetrominoCommand(..))


type Cell
    = Empty
    | Moving ShapeColor
    | Fixed ShapeColor


type alias Row =
    Array Cell


type alias Grid =
    Array Row


type PlayField
    = PlayField (Maybe Tetromino) Grid


type PlayFieldState
    = Playable
    | Full


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


countCellAtState : (Cell -> Bool) -> List ( Int, Int ) -> PlayField -> Int
countCellAtState fn positions (PlayField _ grid) =
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
    countCellAtState isEmptyCell (T.positions tetromino) grid == 4


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
    PlayField (Just tetromino) <| List.foldl (setCellState cell) grid (T.positions tetromino)


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
        ( T.Rotate _, True ) ->
            projectTetrominoToGrid (Moving (T.getColor updatedTetromino)) updatedTetromino cleanedField

        ( T.Rotate (_ as rotateCommand), False ) ->
            case T.whichWallKickToAttempt tetromino of
                Nothing ->
                    playfield

                Just wallKick ->
                    tryWallKick wallKick rotateCommand tetromino playfield

        ( _, True ) ->
            projectTetrominoToGrid (Moving (T.getColor updatedTetromino)) updatedTetromino cleanedField

        ( _, False ) ->
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
    let
        rows =
            Array.toList grid |> List.reverse

        ( cleanedRows, numberOfRemovedLines ) =
            removeFullRows rows [] 0

        updatedGrid =
            List.append (List.repeat numberOfRemovedLines createRow) cleanedRows |> Array.fromList
    in
    ( PlayField Nothing updatedGrid, numberOfRemovedLines )


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
