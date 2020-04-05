module TetrisTests exposing (..)

import Array
import Expect
import Fuzz exposing (list)
import Fuzzing exposing (fuzzTetrisAction)
import Grid exposing (isRowFull)
import Playfield exposing (retrieveGrid)
import ScoringSystem exposing (ScoringSystem)
import Shape exposing (shapeO)
import Test exposing (Test, describe, fuzzWith)
import Tetris exposing (SpawnCommand(..), Tetris, TetrisCommmand(..), applyCommand, startTetris)


suite : Test
suite =
    describe "Score and level mechanics"
        [ fuzzWith { runs = 1000 } (list fuzzTetrisAction) "At any moment of a Tetris game, there should be no full row" <|
            -- Clearly not the best property based test out there but trying :)
            \actions ->
                let
                    tetrisCommands : List TetrisCommmand
                    tetrisCommands =
                        List.concat ([ SpawningTetromino ( shapeO, [] ) ] :: actions)

                    tetris =
                        List.foldl (\cmd t -> Tuple.first <| applyCommand cmd t) startTetris tetrisCommands

                    hasNoFullRow =
                        \t -> not <| List.any isRowFull (t.playfield |> retrieveGrid |> Array.toList)
                in
                Expect.true "Should have no full row" (hasNoFullRow tetris)
        ]
