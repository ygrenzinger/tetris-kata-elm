module TetrisTests exposing (..)

import Expect
import Test exposing (Test, describe, test)
import Tetris exposing (ScoringSystem(..), addRemovedLinesToScoring, initScoring)


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
        ]
