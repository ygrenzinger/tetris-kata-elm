module Tetris exposing (..)

import Playfield as P exposing (PlayField, PlayFieldState(..))
import Shape exposing (Shape, allShapes)
import Tetromino exposing (TetrominoCommand)

type alias Score = Int
type alias AvailableShape = List Shape
type Tetris = Tetris PlayField AvailableShape Score
type SpawnCommand = SpawnRandomShape (List Shape)| Keep

retrieveField : Tetris -> P.PlayField
retrieveField (Tetris field _ _) = field

startTetris : Shape -> Tetris
startTetris shape =
    let
        availableShapes = List.filter (\s -> s == shape) allShapes
    in
        Tetris (P.createPlayfield shape) availableShapes 0

resetAvailableShapes : Tetris -> Tetris
resetAvailableShapes (Tetris field _ score) = Tetris field allShapes score

updateScore : Score -> Int -> Int
updateScore score numberOfRemovedLines = numberOfRemovedLines + score

applyTetrominoCommand : TetrominoCommand -> Tetris -> Tetris
applyTetrominoCommand tetrominoCommand (Tetris field shapes score) =
    Tetris (P.applyCommand tetrominoCommand field) shapes score

makePieceFallDown : Tetris -> (Tetris, SpawnCommand)
makePieceFallDown (Tetris field shapes score) = case (P.pieceFallingDown field) of
        (updatedField, Nothing) -> (Tetris updatedField shapes score, Keep)
        (updatedField, Just numberOfRemovedLines) -> (Tetris updatedField shapes (updateScore score numberOfRemovedLines), SpawnRandomShape shapes)

spawnTetromino : Shape -> Tetris -> (Tetris, PlayFieldState)
spawnTetromino shape (Tetris field availableShapes score) =
    case (P.retrieveGrid field |> P.spawnTetromino shape) of
        (updatedField, P.Full) -> (Tetris updatedField availableShapes score, Full)
        (updatedField, P.Playable) ->
            let
                updatedAvailableShapes = List.filter (\s -> s == shape) availableShapes
            in
                (Tetris updatedField updatedAvailableShapes score, Playable)


