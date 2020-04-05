module ScoringSystem exposing (..)

import Dict exposing (Dict)


type alias ScoringSystem =
    { score : Int
    , level : Int
    , counter : Int
    }


initScoring : ScoringSystem
initScoring =
    ScoringSystem 0 1 0


counterDict : Dict Int Int
counterDict =
    Dict.fromList
        [ ( 1, 1 )
        , ( 2, 3 )
        , ( 3, 5 )
        , ( 4, 8 )
        ]


scoreDict : Dict Int Int
scoreDict =
    Dict.fromList
        [ ( 1, 40 )
        , ( 2, 100 )
        , ( 3, 300 )
        , ( 4, 1200 )
        ]


addRemovedLinesToScoring : Int -> ScoringSystem -> ScoringSystem
addRemovedLinesToScoring numberOfRemovedLines { score, level, counter } =
    let
        updatedCounter =
            counter
                + (Dict.get numberOfRemovedLines counterDict |> Maybe.withDefault 0)

        scoreByLines =
            Dict.get numberOfRemovedLines scoreDict |> Maybe.withDefault 0

        updatedScore =
            score + (scoreByLines * level)

        updatedLevel =
            floor (toFloat updatedCounter / (5 * toFloat level)) + 1
    in
    ScoringSystem updatedScore updatedLevel updatedCounter
