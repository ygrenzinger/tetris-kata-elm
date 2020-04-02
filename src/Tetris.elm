module Tetris exposing (..)

import Playfield as P exposing (PlayField, PlayFieldState(..))
import Shape exposing (Shape, allShapes)
import Tetromino exposing (TetrominoCommand)


type alias Score =
    Int


type alias AvailableShape =
    List Shape


type Tetris
    = Tetris PlayField AvailableShape Score


type SpawnCommand
    = SpawnRandomShape (List Shape)
    | Keep


retrieveField : Tetris -> P.PlayField
retrieveField (Tetris field _ _) =
    field


startTetris : Tetris
startTetris =
    Tetris P.createPlayfield allShapes 0


resetAvailableShapes : Tetris -> Tetris
resetAvailableShapes (Tetris field _ score) =
    Tetris field allShapes score


retrieveAvailableShapes : Tetris -> List Shape
retrieveAvailableShapes (Tetris _ shapes _) =
    shapes


updateScore : Score -> Int -> Int
updateScore score numberOfRemovedLines =
    numberOfRemovedLines + score


applyTetrominoCommand : TetrominoCommand -> Tetris -> Tetris
applyTetrominoCommand tetrominoCommand (Tetris field shapes score) =
    Tetris (P.applyCommand tetrominoCommand field) shapes score


makePieceFallDown : Tetris -> ( Tetris, SpawnCommand )
makePieceFallDown (Tetris field shapes score) =
    case P.makeTetrominoFallDown field of
        ( updatedField, Nothing ) ->
            ( Tetris updatedField shapes score, Keep )

        ( updatedField, Just numberOfRemovedLines ) ->
            ( Tetris updatedField shapes (updateScore score numberOfRemovedLines), SpawnRandomShape shapes )


spawnTetromino : Shape -> List Shape -> Tetris -> ( Tetris, PlayFieldState )
spawnTetromino shape availableShapes (Tetris field _ score) =
    case P.spawnTetromino shape field of
        ( updatedField, P.Full ) ->
            ( Tetris updatedField availableShapes score, Full )

        ( updatedField, P.Playable ) ->
            ( Tetris updatedField availableShapes score, Playable )
