module Tetris exposing (..)

import Playfield exposing (Grid, PlayField, createPlayfield, applyCommand)
import Shape exposing (Shape, allShapes)

type alias Score = Int
type alias AvailableShape = List Shape
type Tetris = Tetris PlayField AvailableShape Score

type TetrisCommand = StartTetris Shape | SpawnTetromino Shape

retrieveField : Tetris -> PlayField
retrieveField (Tetris field _ _) = field

startTetris : Shape -> Tetris
startTetris shape =
    let
        availableShapes = List.filter (\s -> s == shape) allShapes
    in
        Tetris (createPlayfield shape) availableShapes 0

applyOnField : (PlayField -> PlayField) -> Tetris -> Tetris
applyOnField f (Tetris field shapes score) = Tetris (f field) shapes score

