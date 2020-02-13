module Tetromino exposing (..)

type alias TetrominoShape = List (List Bool)

shapeI : TetrominoShape
shapeI = [[True,True,True,True]]

shapeJ : TetrominoShape
shapeJ = [[True,False,False],[True,True,True]]

shapeL : TetrominoShape
shapeL = [[False,False,True],[True,True,True]]

shapeO : TetrominoShape
shapeO = [[True,True],[True,True]]

shapeS : TetrominoShape
shapeS = [[False,True,True],[True,True,False]]

shapeZ : TetrominoShape
shapeZ = [[True,True,False],[False,True,True]]

shapeT : TetrominoShape
shapeT = [[False,True,False],[True,True,True]]

shapes : List TetrominoShape
shapes = [shapeI, shapeJ, shapeL, shapeO, shapeS, shapeZ, shapeT]

rotateClockWise : TetrominoShape -> TetrominoShape
rotateClockWise shape =
    let heads = List.map (List.take 1) shape |> List.concat |> List.reverse
        tails = List.map (List.drop 1) shape
    in
        if List.isEmpty heads then []
        else heads::(rotateClockWise tails)

rotateCounterClockWise : TetrominoShape -> TetrominoShape
rotateCounterClockWise shape =
    let heads = List.map (List.take 1) shape |> List.concat
        tails = List.map (List.drop 1) shape
    in
        if List.isEmpty heads then []
        else heads::(rotateCounterClockWise tails) |> List.reverse
