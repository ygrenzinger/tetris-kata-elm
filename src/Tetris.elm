module Tetris exposing (..)

import Playfield as P exposing (Playfield, PlayfieldState(..))
import ScoringSystem exposing (ScoringSystem(..), addRemovedLinesToScoring, initScoring)
import Shape exposing (Shape, TetrominoShape, allShapes)
import Tetromino exposing (TetrominoCommand(..))


type alias AvailableShape =
    List TetrominoShape


type Tetris
    = Tetris Playfield AvailableShape ScoringSystem


type SpawnCommand
    = SpawnRandomShape (List TetrominoShape)
    | Keep


retrieveField : Tetris -> P.Playfield
retrieveField (Tetris field _ _) =
    field


startTetris : Tetris
startTetris =
    Tetris P.createPlayfield allShapes initScoring


retrieveScore : Tetris -> ScoringSystem
retrieveScore (Tetris _ _ scoring) =
    scoring


timeSpentInRow : Tetris -> Float
timeSpentInRow (Tetris _ _ (ScoringSystem _ level _)) =
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


spawnTetromino : TetrominoShape -> List TetrominoShape -> Tetris -> ( Tetris, PlayfieldState )
spawnTetromino shape availableShapes (Tetris field _ score) =
    case P.spawnTetromino shape field of
        ( updatedField, P.Full ) ->
            ( Tetris updatedField availableShapes score, Full )

        ( updatedField, P.Playable ) ->
            ( Tetris updatedField availableShapes score, Playable )
