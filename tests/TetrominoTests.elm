module TetrominoTests exposing (..)

import Expect
import Fuzzing exposing (fuzzShape)
import Shape exposing (cellPositions, fromStringRepresentation, rotateClockWise, rotateCounterClockWise, shapeL, shapeT)
import Test exposing (..)

suite : Test
suite =
    describe "Tetromino tests"
        [ test "Retrieve Cell positions" <|
                \_ -> Expect.equal [(0,1),(1,0),(1,1),(1,2)] (cellPositions shapeT)
          , test "rotating shape 90° clockwise" <|
                \_ ->
                    let
                      rotatedShape = fromStringRepresentation [
                            " X ",
                            " X ",
                            " XX"
                        ]
                    in
                      Expect.equal rotatedShape (rotateClockWise shapeL)
           , fuzz fuzzShape  "rotating shape 90° clockwise then counterclockwise" <|
                \shape -> Expect.equal shape ((rotateClockWise >> rotateCounterClockWise) shape)
           , fuzz fuzzShape "rotating shape 90° clockwise four times" <|
                \shape ->
                     let
                         fourTimesRotation = (rotateClockWise >> rotateClockWise >> rotateClockWise >> rotateClockWise)
                     in
                         Expect.equal shape (fourTimesRotation shape)
        ]