module TetrominoTests exposing (..)

import Expect
import Fuzzing exposing (fuzzShape)
import Shape exposing (TetrominoShape(..), cellPositions, fromStringRepresentation, rotateClockWise, rotateCounterClockWise, shapeI, shapeL, shapeT)
import Test exposing (..)
import Tetromino exposing (Tetromino(..), WallKick(..), moveTetrominoLeft, moveTetrominoRight, rotateTetrominoLeft, rotateTetrominoRight, samePosition, whichWallKickToAttempt)


suite : Test
suite =
    describe "Tetromino tests"
        [ test "Retrieve Cell positions" <|
            \_ -> Expect.equal [ ( 0, 1 ), ( 1, 0 ), ( 1, 1 ), ( 1, 2 ) ] (cellPositions shapeT)
        , test "rotating shape 90° clockwise" <|
            \_ ->
                let
                    exepectedShape =
                        fromStringRepresentation
                            [ " X "
                            , " X "
                            , " XX"
                            ]

                    (TetrominoShape _ _ rotatedShape) =
                        rotateClockWise shapeL
                in
                Expect.equal exepectedShape rotatedShape
        , fuzz fuzzShape "rotating shape 90° clockwise then counterclockwise" <|
            \shape -> Expect.equal shape ((rotateClockWise >> rotateCounterClockWise) shape)
        , fuzz fuzzShape "rotating shape 90° clockwise four times" <|
            \shape ->
                let
                    fourTimesRotation =
                        rotateClockWise >> rotateClockWise >> rotateClockWise >> rotateClockWise
                in
                Expect.equal shape (fourTimesRotation shape)
        , test "Right Wall Kick possible" <|
            \_ ->
                let
                    tetromino =
                        rotateTetrominoLeft (Tetromino shapeI ( 0, 3 ))

                    movedTetromino =
                        List.foldl (\f t -> f t) tetromino (List.repeat 5 moveTetrominoRight)
                in
                Expect.equal (Just (RightWallKick 2)) (whichWallKickToAttempt movedTetromino)
        , test "Left Wall Kick possible" <|
            \_ ->
                let
                    tetromino =
                        rotateTetrominoRight (Tetromino shapeI ( 0, 3 ))

                    movedTetromino =
                        List.foldl (\f t -> f t) tetromino (List.repeat 5 moveTetrominoLeft)
                in
                Expect.equal (Just (LeftWallKick 2)) (whichWallKickToAttempt movedTetromino)
        , test "Same position" <|
            \_ ->
                Expect.true "Same position" (samePosition (Tetromino shapeI ( 18, 6 )) (Tetromino shapeI ( 18, 6 )))
        ]
