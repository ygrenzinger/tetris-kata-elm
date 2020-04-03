module TetrisTests exposing (..)

import Expect
import Test exposing (Test, describe, test)
import Tetris exposing (updateScore)


suite : Test
suite =
    describe "Playfield mechanics"
        [ test "Spawn piece at the top of the playfield" <|
            \_ ->
                let
                    originalScore =
                        1000

                    inputs =
                        [ ( 0, 1 ), ( 1, 1 ), ( 2, 1 ), ( 3, 1 ), ( 4, 1 ), ( 4, 2 ) ]
                in
                Expect.equal [ 1000, 1040, 1100, 1300, 2200, 3400 ] (List.map (\( lines, level ) -> updateScore lines originalScore level) inputs)
        ]
