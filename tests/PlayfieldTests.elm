module PlayfieldTests exposing (..)

import Expect
import Fuzz exposing (list)
import Fuzzing exposing (fuzzMoveCommand, fuzzShape)
import Playfield exposing (..)
import Shape exposing (Shape, shapeI, shapeO)
import Test exposing (..)
import Tetromino exposing (MoveCommand(..), RotateCommand(..), Tetromino(..), TetrominoCommand(..))


buildPositions : ( Int, Int ) -> ( Int, Int ) -> List ( Int, Int )
buildPositions ( iRow, jRow ) ( iColumn, jColumn ) =
    List.concatMap (\i -> List.map (\j -> ( i, j )) (List.range iColumn jColumn)) (List.range iRow jRow)


updateGrid : Cell -> List ( Int, Int ) -> Grid -> Grid
updateGrid cell positions grid =
    List.foldl (setCellState cell) grid positions


countMovingCell : ( Int, Int ) -> ( Int, Int ) -> PlayField -> Int
countMovingCell rowRange columnRange =
    countCellAtState Moving (buildPositions rowRange columnRange)


createPlayFieldWithShape : Shape -> PlayField
createPlayFieldWithShape shape =
    createPlayfield |> spawnTetromino shape |> Tuple.first


makeTetrominoFallDownUntilBlocked : ( PlayField, Maybe Int ) -> ( PlayField, Maybe Int )
makeTetrominoFallDownUntilBlocked ( PlayField tetromino grid, lines ) =
    case tetromino of
        Nothing ->
            ( PlayField tetromino grid, lines )

        _ ->
            makeTetrominoFallDownUntilBlocked (makeTetrominoFallDown (PlayField tetromino grid))


suite : Test
suite =
    describe "Playfield mechanics"
        [ fuzz fuzzShape "Spawn piece at the top of the playfield" <|
            \shape ->
                let
                    nbMovingBlocks =
                        countCellAtState Moving (buildPositions ( 0, 1 ) ( 3, 7 )) (createPlayFieldWithShape shape)
                in
                Expect.equal 4 nbMovingBlocks
        , test "Moving tetromino at the far left" <|
            \_ ->
                let
                    field =
                        List.foldl applyCommand (createPlayFieldWithShape shapeO) (List.repeat 10 (Move MoveLeft))
                in
                Expect.equal 4 (countMovingCell ( 0, 1 ) ( 0, 1 ) field)
        , test "Moving tetromino at the far right" <|
            \_ ->
                let
                    field =
                        List.foldl applyCommand (createPlayFieldWithShape shapeO) (List.repeat 10 (Move MoveRight))
                in
                Expect.equal 4 (countMovingCell ( 0, 1 ) ( 8, 9 ) field)
        , test "Moving tetromino to the bottom" <|
            \_ ->
                let
                    field =
                        List.foldl applyCommand (createPlayFieldWithShape shapeO) (List.repeat 20 (Move MoveDown))
                in
                Expect.equal 4 (countMovingCell ( 18, 19 ) ( 3, 7 ) field)
        , fuzz (list fuzzMoveCommand) "Whatever the move, there always should be only 4 moving cells" <|
            \commands ->
                let
                    field =
                        List.foldl applyCommand (createPlayFieldWithShape shapeO) commands
                in
                Expect.equal 4 (countMovingCell ( 0, 19 ) ( 0, 9 ) field)
        , test "Rotating tetromino on the left" <|
            \_ ->
                let
                    field =
                        applyCommand (Rotate RotateLeft) <| createPlayFieldWithShape shapeI
                in
                Expect.equal 4 (countMovingCell ( 0, 3 ) ( 4, 4 ) field)
        , test "Rotating tetromino on the right" <|
            \_ ->
                let
                    field =
                        applyCommand (Rotate RotateRight) <| createPlayFieldWithShape shapeI
                in
                Expect.equal 4 (countMovingCell ( 0, 3 ) ( 5, 5 ) field)
        , test "doing a left kick wall" <|
            \_ ->
                let
                    createdField =
                        applyCommand (Rotate RotateRight) <| createPlayFieldWithShape shapeI

                    pieceOnTheLeft =
                        List.foldl applyCommand createdField (List.repeat 10 (Move MoveLeft))

                    kickedFromTheWall =
                        applyCommand (Rotate RotateLeft) pieceOnTheLeft
                in
                Expect.equal 4 (countMovingCell ( 1, 1 ) ( 0, 4 ) kickedFromTheWall)
        , test "doing a right kick wall" <|
            \_ ->
                let
                    createdField =
                        applyCommand (Rotate RotateLeft) <| createPlayFieldWithShape shapeI

                    pieceOnTheRight =
                        List.foldl applyCommand createdField (List.repeat 10 (Move MoveRight))

                    kickedFromTheWall =
                        applyCommand (Rotate RotateRight) pieceOnTheRight
                in
                Expect.equal 4 (countMovingCell ( 1, 1 ) ( 6, 9 ) kickedFromTheWall)
        , test "Move tetromino until blocked by fixed cells" <|
            \_ ->
                let
                    grid =
                        createGrid
                            |> updateGrid Fixed (buildPositions ( 18, 19 ) ( 0, 9 ))

                    ( originalField, _ ) =
                        PlayField Nothing grid |> spawnTetromino shapeO

                    field =
                        List.foldl applyCommand originalField (List.repeat 20 (Move MoveDown))
                in
                Expect.equal 4 (countMovingCell ( 16, 17 ) ( 0, 9 ) field)
        , test "make a tetromino fall until blocked and fix it" <|
            \_ ->
                let
                    originalField =
                        createPlayFieldWithShape shapeI

                    result =
                        makeTetrominoFallDownUntilBlocked ( originalField, Nothing )

                    nbTotalFixedBlocks =
                        Tuple.first result |> countCellAtState Fixed (buildPositions ( 0, 19 ) ( 0, 9 ))
                in
                Expect.equal ( 4, Just 0 ) ( nbTotalFixedBlocks, Tuple.second result )
        , test "make a tetromino fall until blocked, remove full lines and count them" <|
            \_ ->
                let
                    grid =
                        createGrid
                            |> updateGrid Fixed (buildPositions ( 17, 19 ) ( 0, 9 ))
                            |> updateGrid Empty (buildPositions ( 17, 19 ) ( 4, 5 ))

                    ( originalField, _ ) =
                        PlayField Nothing grid |> spawnTetromino shapeO

                    ( field, removedLines ) =
                        makeTetrominoFallDownUntilBlocked ( originalField, Nothing )

                    nbTotalFixedBlocks =
                        countCellAtState Fixed (buildPositions ( 0, 19 ) ( 0, 9 )) field

                    nbBottomLinesFixedBlocks =
                        countCellAtState Fixed (buildPositions ( 19, 19 ) ( 0, 9 )) field
                in
                Expect.equal ( 8, 8, Just 2 ) ( nbTotalFixedBlocks, nbBottomLinesFixedBlocks, removedLines )
        ]
