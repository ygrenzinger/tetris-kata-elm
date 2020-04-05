module Tetris exposing (..)

import Playfield as P exposing (Playfield(..))
import ScoringSystem exposing (ScoringSystem, addRemovedLinesToScoring, initScoring)
import Shape exposing (Shape, TetrominoShape, allShapes)
import Tetromino exposing (TetrominoCommand(..))


type alias Tetris =
    { playfield : Playfield, availableShapes : List TetrominoShape, scoringSystem : ScoringSystem }


type SpawnCommand
    = SpawnRandomShape (List TetrominoShape)
    | None


type TetrisCommmand
    = TetrominoCommand TetrominoCommand
    | SpawningTetromino ( TetrominoShape, List TetrominoShape )
    | FallingDown


startTetris : Tetris
startTetris =
    { playfield = P.createPlayfield, availableShapes = allShapes, scoringSystem = initScoring }


isGameOver : Tetris -> Bool
isGameOver { playfield } =
    case playfield of
        Full _ ->
            True

        _ ->
            False


timeSpentInRow : Tetris -> Float
timeSpentInRow { scoringSystem } =
    (0.8 - ((toFloat scoringSystem.level - 1) * 0.007)) ^ (toFloat scoringSystem.level - 1) * 1000


applyCommand : TetrisCommmand -> Tetris -> ( Tetris, SpawnCommand )
applyCommand command tetris =
    case command of
        TetrominoCommand tetrominoCommand ->
            ( { tetris | playfield = P.applyCommand tetrominoCommand tetris.playfield }, None )

        FallingDown ->
            makePieceFallDown tetris

        SpawningTetromino ( shape, availableShape ) ->
            ( { playfield = P.spawnTetromino shape tetris.playfield, availableShapes = availableShape, scoringSystem = tetris.scoringSystem }, None )


makePieceFallDown : Tetris -> ( Tetris, SpawnCommand )
makePieceFallDown tetris =
    case P.makeTetrominoFallDown tetris.playfield of
        ( updatedField, Nothing ) ->
            ( { tetris | playfield = updatedField }, None )

        ( updatedField, Just numberOfRemovedLines ) ->
            let
                updatedScoringSystem =
                    addRemovedLinesToScoring numberOfRemovedLines tetris.scoringSystem
            in
            ( { playfield = updatedField, availableShapes = tetris.availableShapes, scoringSystem = updatedScoringSystem }, SpawnRandomShape tetris.availableShapes )
