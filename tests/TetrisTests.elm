module TetrisTests exposing (..)

import Array
import Expect
import Fuzz exposing (list)
import Fuzzing exposing (fuzzTetrisAction)
import Playfield exposing (PlayFieldState(..), isRowFull, retrieveGrid)
import Shape exposing (shapeO)
import Test exposing (Test, describe, fuzz, fuzzWith, test)
import Tetris exposing (ScoringSystem(..), Tetris, addRemovedLinesToScoring, initScoring, retrieveField, retrieveScore, spawnTetromino, startTetris)


suite : Test
suite =
    describe "Score and level mechanics"
        [ test "Increase level" <|
            \_ ->
                let
                    originalScore =
                        Scoring 0 1 0

                    (Scoring _ level _) =
                        addRemovedLinesToScoring 4 originalScore
                in
                Expect.equal 2 level
        , test "Increase scoring system" <|
            \_ ->
                let
                    linesRemoved =
                        [ 1, 2, 3, 4 ]

                    originalScore : ScoringSystem
                    originalScore =
                        initScoring

                    f =
                        \l ( s, list ) ->
                            let
                                score =
                                    addRemovedLinesToScoring l s
                            in
                            ( score, score :: list )

                    scores : List ScoringSystem
                    scores =
                        List.foldl f ( originalScore, [] ) linesRemoved
                            |> Tuple.second
                            |> List.reverse
                in
                Expect.equal [ Scoring 40 1 1, Scoring 140 1 4, Scoring 440 2 9, Scoring 2840 2 17 ] scores
        , fuzzWith { runs = 2000 } (list fuzzTetrisAction) "At any moment of a Tetris game, there should be no full row" <|
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
