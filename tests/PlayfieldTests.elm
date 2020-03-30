module PlayfieldTests exposing (..)

import Array
import Expect
import Fuzz exposing (list)
import Fuzzing exposing (fuzzMoveCommand, fuzzShape)
import Playfield exposing (Cell(..), Grid, PlayField(..), applyCommand, countCellAtState, createPlayfield, fixTetromino, cleanFullLines, retrieveGrid, retrieveTetromino, setCellState, setGrid)
import Shape exposing (shapeI, shapeO)
import Test exposing (..)
import Tetromino exposing (MoveCommand(..), RotateCommand(..), Tetromino(..), TetrominoCommand(..))

buildPositions : (Int, Int) -> (Int, Int) -> List (Int, Int)
buildPositions (iRow, jRow) (iColumn,jColumn) = List.concatMap (\i -> List.map (\j -> (i,j)) (List.range iColumn jColumn)) (List.range iRow jRow)

countMovingCell : (Int, Int) -> (Int, Int) -> PlayField -> Int
countMovingCell rowRange columnRange field = retrieveGrid field |> countCellAtState Moving (buildPositions rowRange columnRange)

suite : Test
suite = describe "Playfield mechanics"
            [
                fuzz fuzzShape "Spawn piece at the top of the playfield" <|
                    \shape ->
                    let
                        grid = createPlayfield shape |> retrieveGrid
                        positions = buildPositions (0,1) (3,7)
                        nbMovingBlocks = countCellAtState Moving positions grid
                    in

                        Expect.equal 4 nbMovingBlocks
                , test "Moving tetromino at the far left" <|
                    \_ ->
                    let
                        field = List.foldl applyCommand (createPlayfield shapeO) (List.repeat 10 (Move MoveLeft))
                     in
                        Expect.equal 4 (countMovingCell (0,1) (0,1) field)
                , test "Moving tetromino at the far right" <|
                    \_ ->
                    let
                        field = List.foldl applyCommand (createPlayfield shapeO) (List.repeat 10 (Move MoveRight))
                     in
                        Expect.equal 4 (countMovingCell (0,1) (8,9) field)
                , test "Moving tetromino to the bottom" <|
                    \_ ->
                    let
                        field = List.foldl applyCommand (createPlayfield shapeO) (List.repeat 20 (Move MoveDown))
                     in
                        Expect.equal 4 (countMovingCell (18,19) (3,7) field)
                , fuzz (list fuzzMoveCommand) "Whatever the move, there always should be only 4 moving cells" <|
                    \commands ->
                    let
                        field = List.foldl applyCommand (createPlayfield shapeO) commands
                     in
                        Expect.equal 4 (countMovingCell (0,19) (0,9) field)
                , test "Move tetromino until blocked by fixed cells" <|
                    \_ ->
                    let
                        originalField = (createPlayfield shapeO)
                        updatedGrid = List.foldl (setCellState Fixed) (retrieveGrid originalField) (buildPositions (18,19) (0,9))
                        field = List.foldl applyCommand (setGrid originalField updatedGrid) (List.repeat 20 (Move MoveDown))
                    in
                        Expect.equal 4 (countMovingCell (16,17) (0,9) field)
                , test "Rotating tetromino on the left" <|
                    \_ ->
                    let
                        field = applyCommand (Rotate RotateLeft) <| createPlayfield shapeI
                     in
                        Expect.equal 4 (countMovingCell (0,3) (4,4) field)
                , test "Rotating tetromino on the right" <|
                    \_ ->
                    let
                        field = applyCommand (Rotate RotateRight) <| createPlayfield shapeI
                     in
                        Expect.equal 4 (countMovingCell (0,3) (5,5) field)
                , test "doing a left kick wall" <|
                    \_ ->
                    let
                        createdField = applyCommand (Rotate RotateRight) <| createPlayfield shapeI
                        pieceOnTheLeft = List.foldl applyCommand createdField (List.repeat 10 (Move MoveLeft))
                        kickedFromTheWall = applyCommand (Rotate RotateLeft) pieceOnTheLeft
                     in
                        Expect.equal 4 (countMovingCell (1,1) (0,4) kickedFromTheWall)
                , test "doing a right kick wall" <|
                    \_ ->
                    let
                        createdField = applyCommand (Rotate RotateLeft) <| createPlayfield shapeI
                        pieceOnTheRight = List.foldl applyCommand createdField (List.repeat 10 (Move MoveRight))
                        kickedFromTheWall = applyCommand (Rotate RotateRight) pieceOnTheRight
                     in
                        Expect.equal 4 (countMovingCell (1,1) (6,9) kickedFromTheWall)
                , test "Fix tetromino" <|
                    \_ ->
                    let
                        field = List.foldl applyCommand (createPlayfield shapeO) (List.repeat 20 (Move MoveDown)) |> fixTetromino
                        positions = buildPositions (18,19) (3,7)
                        nbFixedBlocks = retrieveGrid field |> countCellAtState Fixed positions
                     in
                        Expect.equal 4 nbFixedBlocks
                , test "Remove and count full lines" <|
                    \_ ->
                    let
                        originalField = (createPlayfield shapeO)
                        updatedGrid : Grid
                        updatedGrid = List.foldl (setCellState Fixed) (retrieveGrid originalField) (buildPositions (16,19) (0,9))
                            |> setCellState Empty (17, 0)
                            |> setCellState Fixed (15, 4)
                            |> setCellState Fixed (15, 5)
                            |> setCellState Fixed (14, 4)
                            |> setCellState Fixed (14, 5)
                        (field, nblines) = cleanFullLines (PlayField (retrieveTetromino originalField) updatedGrid)
                        nbTotalFixedBlocks = retrieveGrid field |> countCellAtState Fixed (buildPositions (0,19) (0,9))
                        nbBottomLinesFixedBlocks = retrieveGrid field |> countCellAtState Fixed (buildPositions (19,19) (0,9))
                    in
                        Expect.equal (13,9,3) (nbTotalFixedBlocks, nbBottomLinesFixedBlocks, nblines)

            ]