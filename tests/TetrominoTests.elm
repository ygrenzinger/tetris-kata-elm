module TetrominoTests exposing (..)

import Expect
import Test exposing (..)
import Tetromino exposing (..)

suite : Test
suite =
    describe "Tetromino tests"
        [ test "rotating shape 90° clockwise" <|
            \_ ->
                let
                    rotatedShape = [[True,False],[True,False],[True,True]]
                in
                    Expect.equal rotatedShape (rotateClockWise shapeL)
           , test "rotating shape 90° clockwise then counterclockwise" <|
                         \_ -> Expect.equal shapeL ((rotateClockWise >> rotateCounterClockWise) shapeL)
           , test "rotating shape 90° clockwise four times" <|
                         \_ ->
                         let
                             fourTimesRotation = (rotateClockWise >> rotateClockWise >> rotateClockWise >> rotateClockWise)
                         in
                             Expect.equal shapeL (fourTimesRotation shapeL)
        ]