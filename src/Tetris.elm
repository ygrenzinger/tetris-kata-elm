module Tetris exposing (..)

import Debug exposing (toString)
import Playfield as P exposing (PlayField, PlayFieldState(..))
import Shape exposing (Shape, allShapes)
import Tetromino exposing (TetrominoCommand)


type alias Score =
    Int


type alias Level =
    Int


type alias AvailableShape =
    List Shape


type Tetris
    = Tetris PlayField AvailableShape Score Level


type SpawnCommand
    = SpawnRandomShape (List Shape)
    | Keep


retrieveField : Tetris -> P.PlayField
retrieveField (Tetris field _ _ _) =
    field


startTetris : Tetris
startTetris =
    Tetris P.createPlayfield allShapes 0 1


scoreToString : Tetris -> String
scoreToString (Tetris _ _ score _) =
    toString score


updateScore : Int -> Level -> Score -> Int
updateScore numberOfRemovedLines score level =
    let
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
    in
    score + (scoreByLines * level)


applyTetrominoCommand : TetrominoCommand -> Tetris -> Tetris
applyTetrominoCommand tetrominoCommand (Tetris field shapes score level) =
    Tetris (P.applyCommand tetrominoCommand field) shapes score level


makePieceFallDown : Tetris -> ( Tetris, SpawnCommand )
makePieceFallDown (Tetris field shapes score level) =
    case P.makeTetrominoFallDown field of
        ( updatedField, Nothing ) ->
            ( Tetris updatedField shapes score level, Keep )

        ( updatedField, Just numberOfRemovedLines ) ->
            ( Tetris updatedField shapes (updateScore numberOfRemovedLines level score) level, SpawnRandomShape shapes )


spawnTetromino : Shape -> List Shape -> Tetris -> ( Tetris, PlayFieldState )
spawnTetromino shape availableShapes (Tetris field _ score level) =
    case P.spawnTetromino shape field of
        ( updatedField, P.Full ) ->
            ( Tetris updatedField availableShapes score level, Full )

        ( updatedField, P.Playable ) ->
            ( Tetris updatedField availableShapes score level, Playable )
