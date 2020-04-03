module Tetris exposing (..)

import Debug exposing (toString)
import Playfield as P exposing (PlayField, PlayFieldState(..))
import Shape exposing (Shape, TetrominoShape, allShapes)
import Tetromino exposing (TetrominoCommand(..))


type alias Score =
    Int


type alias Level =
    Int


type alias Counter =
    Int


type ScoringSystem
    = Scoring Score Level Counter


type alias AvailableShape =
    List TetrominoShape


type Tetris
    = Tetris PlayField AvailableShape ScoringSystem


type SpawnCommand
    = SpawnRandomShape (List TetrominoShape)
    | Keep


retrieveField : Tetris -> P.PlayField
retrieveField (Tetris field _ _) =
    field


startTetris : Tetris
startTetris =
    Tetris P.createPlayfield allShapes initScoring


initScoring : ScoringSystem
initScoring =
    Scoring 0 1 0


scoreToString : Tetris -> String
scoreToString (Tetris _ _ (Scoring score _ _)) =
    toString score


levelToString : Tetris -> String
levelToString (Tetris _ _ (Scoring _ level _)) =
    toString level


addRemovedLinesToScoring : Int -> ScoringSystem -> ScoringSystem
addRemovedLinesToScoring numberOfRemovedLines (Scoring score level counter) =
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
    Scoring updatedScore updatedLevel updatedCounter


timeSpentInRow : Tetris -> Float
timeSpentInRow (Tetris _ _ (Scoring _ level _)) =
    (0.8 - ((toFloat level - 1) * 0.007)) ^ (toFloat level - 1) * 1000


applyTetrominoCommand : TetrominoCommand -> Tetris -> Tetris
applyTetrominoCommand tetrominoCommand (Tetris field shapes score) =
    Tetris (P.applyCommand tetrominoCommand field) shapes score


makePieceFallDown : Tetris -> ( Tetris, SpawnCommand )
makePieceFallDown (Tetris field shapes score) =
    case P.makeTetrominoFallDown field of
        ( updatedField, Nothing ) ->
            ( Tetris updatedField shapes score, Keep )

        ( updatedField, Just numberOfRemovedLines ) ->
            ( Tetris updatedField shapes (addRemovedLinesToScoring numberOfRemovedLines score), SpawnRandomShape shapes )


spawnTetromino : TetrominoShape -> List TetrominoShape -> Tetris -> ( Tetris, PlayFieldState )
spawnTetromino shape availableShapes (Tetris field _ score) =
    case P.spawnTetromino shape field of
        ( updatedField, P.Full ) ->
            ( Tetris updatedField availableShapes score, Full )

        ( updatedField, P.Playable ) ->
            ( Tetris updatedField availableShapes score, Playable )
