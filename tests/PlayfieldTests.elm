module PlayfieldTests exposing (..)

import Expect
import Fuzzing exposing (fuzzShape)
import Playfield exposing (Cell(..), PlayField(..), applyCommand, countCellAtState, createGrid, createPlayfield, fixTetromino, retrieveGrid, retrieveTetromino, setCellState)
import Shape exposing (shapeO)
import Test exposing (..)
import Tetromino exposing (Tetromino(..), TetrominoCommand(..), tetrominoPositions)

buildPositions : (Int, Int) -> (Int, Int) -> List (Int, Int)
buildPositions (iRow, jRow) (iColumn,jColumn) = List.concatMap (\i -> List.map (\j -> (i,j)) (List.range iColumn jColumn)) (List.range iRow jRow)

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
                        nbMovingBlocksAtFarLeft = retrieveGrid field |> countCellAtState Moving (buildPositions (0,1) (0,1))
                        nbMovingBlocksInTheGrid = retrieveGrid field |> countCellAtState Moving (buildPositions (0,19) (0,9))
                     in
                        Expect.equal (4,4) (nbMovingBlocksInTheGrid, nbMovingBlocksAtFarLeft)
                , test "Moving tetromino at the far right" <|
                    \_ ->
                    let
                        field = List.foldl applyCommand (createPlayfield shapeO) (List.repeat 10 MoveRight)
                        nbMovingBlocksAtFarRight = retrieveGrid field |> countCellAtState Moving (buildPositions (0,1) (8,9))
                        nbMovingBlocksInTheGrid = retrieveGrid field |> countCellAtState Moving (buildPositions (0,19) (0,9))
                     in
                        Expect.equal (4,4) (nbMovingBlocksInTheGrid, nbMovingBlocksAtFarRight)
                , test "Moving tetromino to the bottom" <|
                    \_ ->
                    let
                        field = List.foldl applyCommand (createPlayfield shapeO) (List.repeat 20 MoveDown)
                        nbMovingBlocksAtBottom = retrieveGrid field |> countCellAtState Moving (buildPositions (18,19) (3,7))
                        nbMovingBlocksInTheGrid = retrieveGrid field |> countCellAtState Moving (buildPositions (0,19) (0,9))
                     in
                        Expect.equal (4, 4) (nbMovingBlocksInTheGrid, nbMovingBlocksAtBottom)
                , skip <| test "Fix tetromino" <|
                    \_ ->
                    let
                        field = List.foldl applyCommand (createPlayfield shapeO) (List.repeat 20 MoveDown) |> fixTetromino
                        positions = buildPositions (18,19) (3,7)
                        nbFixedBlocks = retrieveGrid field |> countCellAtState Fixed positions
                     in
                        Expect.equal 4 nbFixedBlocks
            ]