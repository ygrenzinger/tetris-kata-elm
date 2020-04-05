module Tetris exposing (..)

import Playfield as P exposing (Playfield)
import ScoringSystem exposing (ScoringSystem, addRemovedLinesToScoring, initScoring)
import Shape exposing (Shape, TetrominoShape, allShapes)
import Tetromino exposing (TetrominoCommand(..))


type alias Tetris =
    { playfield : Playfield, availableShape : List TetrominoShape, scoringSystem : ScoringSystem }


type SpawnCommand
    = SpawnRandomShape (List TetrominoShape)
    | Keep


startTetris : Tetris
startTetris =
    { playfield = P.createPlayfield, availableShape = allShapes, scoringSystem = initScoring }


timeSpentInRow : Tetris -> Float
timeSpentInRow { scoringSystem } =
    (0.8 - ((toFloat scoringSystem.level - 1) * 0.007)) ^ (toFloat scoringSystem.level - 1) * 1000


applyTetrominoCommand : TetrominoCommand -> Tetris -> Tetris
applyTetrominoCommand tetrominoCommand tetris =
    { tetris | playfield = P.applyCommand tetrominoCommand tetris.playfield }


makePieceFallDown : Tetris -> ( Tetris, SpawnCommand )
makePieceFallDown tetris =
    case P.makeTetrominoFallDown tetris.playfield of
        ( updatedField, Nothing ) ->
            ( { tetris | playfield = updatedField }, Keep )

        ( updatedField, Just numberOfRemovedLines ) ->
            let
                updatedScoringSystem =
                    addRemovedLinesToScoring numberOfRemovedLines tetris.scoringSystem
            in
            ( { playfield = updatedField, availableShape = tetris.availableShape, scoringSystem = updatedScoringSystem }, SpawnRandomShape tetris.availableShape )


spawnTetromino : TetrominoShape -> List TetrominoShape -> Tetris -> Tetris
spawnTetromino shape availableShapes tetris =
    { playfield = P.spawnTetromino shape tetris.playfield, availableShape = availableShapes, scoringSystem = tetris.scoringSystem }
