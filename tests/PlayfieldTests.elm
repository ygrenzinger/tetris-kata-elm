module PlayfieldTests exposing (..)

import Expect
import Fuzz exposing (list)
import Fuzzing exposing (fuzzMoveCommand, fuzzShape)
import Playfield exposing (Cell(..), PlayField(..), applyCommand, countCellAtState, createPlayfield, fixTetromino, retrieveGrid, setCellState, setGrid)
import Shape exposing (shapeI, shapeO)
import Test exposing (..)
import Tetromino exposing (Tetromino(..), TetrominoCommand(..))

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
                        field = List.foldl applyCommand (createPlayfield shapeO) (List.repeat 10 MoveLeft)
                     in
                        Expect.equal 4 (countMovingCell (0,1) (0,1) field)
                , test "Moving tetromino at the far right" <|
                    \_ ->
                    let
                        field = List.foldl applyCommand (createPlayfield shapeO) (List.repeat 10 MoveRight)
                     in
                        Expect.equal 4 (countMovingCell (0,1) (8,9) field)
                , test "Moving tetromino to the bottom" <|
                    \_ ->
                    let
                        field = List.foldl applyCommand (createPlayfield shapeO) (List.repeat 20 MoveDown)
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
                        field = List.foldl applyCommand (setGrid originalField updatedGrid) (List.repeat 20 MoveDown)
                    in
                        Expect.equal 4 (countMovingCell (16,17) (0,9) field)
                , test "Rotating tetromino on the left" <|
                    \_ ->
                    let
                        field = applyCommand RotateLeft <| createPlayfield shapeI
                     in
                        Expect.equal 4 (countMovingCell (0,3) (4,4) field)
                , test "Rotating tetromino on the right" <|
                    \_ ->
                    let
                        field = applyCommand RotateRight <| createPlayfield shapeI
                     in
                        Expect.equal 4 (countMovingCell (0,3) (5,5) field)
                , test "Fix tetromino" <|
                    \_ ->
                    let
                        field = List.foldl applyCommand (createPlayfield shapeO) (List.repeat 20 MoveDown) |> fixTetromino
                        positions = buildPositions (18,19) (3,7)
                        nbFixedBlocks = retrieveGrid field |> countCellAtState Fixed positions
                     in
                        Expect.equal 4 nbFixedBlocks
            ]