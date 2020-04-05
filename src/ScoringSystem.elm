module ScoringSystem exposing (..)


type alias Score =
    Int


type alias Level =
    Int


type alias Counter =
    Int


type alias ScoringSystem =
    { score : Int
    , level : Int
    , counter : Int
    }


initScoring : ScoringSystem
initScoring =
    ScoringSystem 0 1 0


addRemovedLinesToScoring : Int -> ScoringSystem -> ScoringSystem
addRemovedLinesToScoring numberOfRemovedLines { score, level, counter } =
    let
        updatedCounter =
            counter
                + (case numberOfRemovedLines of
                    1 ->
                        1

                    2 ->
                        3

                    3 ->
                        5

                    4 ->
                        8

                    _ ->
                        0
                  )

        scoreByLines =
            case numberOfRemovedLines of
                1 ->
                    40

                2 ->
                    100

                3 ->
                    300

                4 ->
                    1200

                _ ->
                    0

        updatedScore =
            score + (scoreByLines * level)

        updatedLevel =
            floor (toFloat updatedCounter / (5 * toFloat level)) + 1
    in
    ScoringSystem updatedScore updatedLevel updatedCounter
