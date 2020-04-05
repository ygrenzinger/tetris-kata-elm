module TetrisTests exposing (..)

import Expect
import ScoringSystem exposing (ScoringSystem(..), addRemovedLinesToScoring, initScoring)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Score and level mechanics"
        [ test "Increase level" <|
            \_ ->
                let
                    originalScore =
                        ScoringSystem 0 1 0

                    (ScoringSystem _ level _) =
                        addRemovedLinesToScoring 4 originalScore
                in
                Expect.equal 2 level
        , test "Increase scoring system" <|
            \_ ->
                let
                    linesRemoved =
                        [ 1, 2, 3, 4 ]

                    f =
                        \l ( s, list ) ->
                            let
                                score =
                                    addRemovedLinesToScoring l s
                            in
                            ( score, score :: list )

                    scores : List ScoringSystem
                    scores =
                        List.foldl f ( initScoring, [] ) linesRemoved
                            |> Tuple.second
                            |> List.reverse
                in
                Expect.equal [ ScoringSystem 40 1 1, ScoringSystem 140 1 4, ScoringSystem 440 2 9, ScoringSystem 2840 2 17 ] scores
        ]
