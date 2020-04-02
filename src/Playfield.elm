module Playfield exposing (..)

import Array exposing (Array, repeat)
import Shape exposing (Shape, shapeSize)
import Tetromino as T exposing (MoveCommand(..), Tetromino(..), TetrominoCommand(..))


type Cell
    = Empty
    | Moving
    | Fixed


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


isRowFullOf : Cell -> Row -> Bool
isRowFullOf cell =
    List.all (\c -> c == cell) << Array.toList


countCellAtState : Cell -> List ( Int, Int ) -> PlayField -> Int
countCellAtState state positions (PlayField _ grid) =
    List.map
        (\pos ->
            if getCellState grid pos == Just state then
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
    Array.repeat 20 createRow


createPlayfield : PlayField
createPlayfield =
    PlayField Nothing createGrid


retrieveGrid : PlayField -> Grid
retrieveGrid (PlayField _ grid) =
    grid


isPossiblePosition : Tetromino -> PlayField -> Bool
isPossiblePosition tetromino grid =
    countCellAtState Empty (T.positions tetromino) grid == 4


spawnTetromino : Shape -> PlayField -> ( PlayField, PlayFieldState )
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
        ( projectTetrominoToGrid Moving tetromino (retrieveGrid field), Playable )

    else
        ( field, Full )


projectTetrominoToGrid : Cell -> Tetromino -> Grid -> PlayField
projectTetrominoToGrid cell tetromino grid =
    PlayField (Just tetromino) <| List.foldl (setCellState cell) grid (T.positions tetromino)


applyCommand : TetrominoCommand -> PlayField -> PlayField
applyCommand command playfield =
    case playfield of
        PlayField (Just tetromino) grid ->
            applyCommandOnTetromino command tetromino grid

        (PlayField Nothing _) as p ->
            p


applyCommandOnTetromino : TetrominoCommand -> Tetromino -> Grid -> PlayField
applyCommandOnTetromino command tetromino grid =
    let
        updatedTetromino =
            T.applyCommand command tetromino

        cleanedField =
            projectTetrominoToGrid Empty tetromino grid

        isPossible =
            isPossiblePosition updatedTetromino cleanedField
    in
    case ( command, isPossible ) of
        ( T.Rotate _, True ) ->
            projectTetrominoToGrid Moving updatedTetromino <| retrieveGrid cleanedField

        ( T.Rotate (_ as rotateCommand), False ) ->
            case T.whichWallKickToAttempt tetromino of
                Nothing ->
                    PlayField (Just tetromino) grid

                Just wallKick ->
                    tryWallKick wallKick rotateCommand tetromino grid

        ( _, True ) ->
            projectTetrominoToGrid Moving updatedTetromino <| retrieveGrid cleanedField

        ( _, False ) ->
            PlayField (Just tetromino) grid


tryWallKick : T.WallKick -> T.RotateCommand -> Tetromino -> Grid -> PlayField
tryWallKick wallKick command tetromino grid =
    let
        moveFn : Tetromino -> Tetromino
        moveFn =
            case wallKick of
                T.LeftWallKick i ->
                    List.foldl (\f g -> f << g) T.moveTetrominoRight (List.repeat (i - 1) T.moveTetrominoRight)

                T.RightWallKick i ->
                    List.foldl (\f g -> f << g) T.moveTetrominoLeft (List.repeat (i - 1) T.moveTetrominoLeft)

        updatedTetromino =
            moveFn tetromino
                |> case command of
                        T.RotateLeft ->
                            T.rotateTetrominoLeft

                        T.RotateRight ->
                            T.rotateTetrominoRight

        cleanedField =
            projectTetrominoToGrid Empty tetromino grid

        isPossible =
            isPossiblePosition updatedTetromino cleanedField
    in
    if isPossible then
        projectTetrominoToGrid Moving updatedTetromino <| retrieveGrid cleanedField

    else
        PlayField (Just tetromino) grid


makeTetrominoFallDown : PlayField -> ( PlayField, Maybe Int )
makeTetrominoFallDown (PlayField tetromino grid) =
    case tetromino of
        Nothing ->
            ( PlayField tetromino grid, Nothing )

        Just t ->
            tetrominoFallDown t grid


tetrominoFallDown : Tetromino -> Grid -> ( PlayField, Maybe Int )
tetrominoFallDown tetromino grid =
    let
        (PlayField updatedTetromino updatedGrid) =
            applyCommandOnTetromino (Move MoveDown) tetromino grid
    in
    if Maybe.map (T.samePosition tetromino) updatedTetromino |> Maybe.withDefault False then
        fixPieceFallingDown tetromino updatedGrid

    else
        ( PlayField updatedTetromino updatedGrid, Nothing )


fixPieceFallingDown : Tetromino -> Grid -> ( PlayField, Maybe Int )
fixPieceFallingDown tetromino grid =
    projectTetrominoToGrid Fixed tetromino grid
        |> cleanFullLinesAndRemoveTetromino
        |> Tuple.mapSecond Just


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
            if isRowFullOf Fixed head then
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
