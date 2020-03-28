module PlayfieldTests exposing (..)

import Expect
import Fuzzing exposing (fuzzShape)
import Playfield exposing (Cell(..), PlayField(..), applyCommand, countCellAtState, createPlayfield, retrieveGrid)
import Shape exposing (shapeO)
import Test exposing (..)
import Tetromino exposing (TetrominoCommand(..))

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
                        positions = buildPositions (0,1) (0,1)
                        nbMovingBlocks = retrieveGrid field |> countCellAtState Moving positions
                     in
                        Expect.equal 4 nbMovingBlocks
                , test "Moving tetromino at the far right" <|
                    \_ ->
                    let
                        field = List.foldl applyCommand (createPlayfield shapeO) (List.repeat 10 MoveRight)
                        positions = buildPositions (0,1) (8,9)
                        nbMovingBlocks = retrieveGrid field |> countCellAtState Moving positions
                     in
                        Expect.equal 4 nbMovingBlocks
                , test "Moving tetromino to the bottom" <|
                    \_ ->
                    let
                        field = List.foldl applyCommand (createPlayfield shapeO) (List.repeat 20 MoveDown)
                        positions = buildPositions (18,19) (3,7)
                        nbMovingBlocks = retrieveGrid field |> countCellAtState Moving positions
                     in
                        Expect.equal 4 nbMovingBlocks
            ]