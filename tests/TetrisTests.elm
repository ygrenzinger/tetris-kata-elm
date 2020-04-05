module TetrisTests exposing (..)

import Array
import Expect
import Fuzz exposing (list)
import Fuzzing exposing (fuzzTetrisAction)
import Grid exposing (isRowFull)
import Playfield exposing (PlayFieldState(..), retrieveGrid)
import Shape exposing (shapeO)
import Test exposing (Test, describe, fuzzWith)
import Tetris exposing (Tetris, retrieveField, spawnTetromino, startTetris)


suite : Test
suite =
    describe "Score and level mechanics"
        [ fuzzWith { runs = 1 } (list fuzzTetrisAction) "At any moment of a Tetris game, there should be no full row" <|
            -- Clearly not the best property based test out there but trying :)
            \actions ->
                let
                    init : Tetris
                    init =
                        startTetris |> spawnTetromino shapeO [] |> Tuple.first

                    tetris : Tetris
                    tetris =
                        List.foldl (\f t -> f t) init actions

                    hasNoFullRow : Tetris -> Bool
                    hasNoFullRow =
                        \t -> not <| List.any isRowFull (retrieveField t |> retrieveGrid |> Array.toList)
                in
                Expect.true "Should have no full row" (hasNoFullRow tetris)
        ]
