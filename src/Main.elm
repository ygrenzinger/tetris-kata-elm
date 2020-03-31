module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--


import Array
import Browser
import Css exposing (..)
import Html.Styled exposing (Html, button, div, text, toUnstyled)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Keyboard exposing (Key(..), KeyChange(..), RawKey)
import Playfield exposing (Cell(..), Grid, Row, applyCommand, retrieveGrid)
import Random
import Shape exposing (Shape, allShapes, randomShapeGenerator)
import Tetris exposing (Tetris, applyOnField, retrieveField, startTetris)
import Tetromino exposing (MoveCommand(..), RotateCommand(..), TetrominoCommand(..))



-- MAIN


main =
  Browser.element {
          init = init
        , update = update
        , subscriptions = subscriptions
        , view = view >> toUnstyled
    }



-- MODEL

type alias Model = { game : Maybe Tetris }

init : () -> (Model, Cmd Msg)
init _ = ( { game = Nothing}, Cmd.none)

-- UPDATE


type Msg = KeyDown RawKey
    | StartGame
    | NewGame (Maybe Shape)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        StartGame -> (model, Random.generate NewGame (randomShapeGenerator allShapes))
        NewGame Nothing -> (model, Cmd.none)
        NewGame (Just shape) -> ({game = Just <| startTetris shape}, Cmd.none)
        KeyDown rawKey -> (applyKeyPress model rawKey, Cmd.none)

applyKeyPress : Model -> RawKey -> Model
applyKeyPress {game} rawkey = case (keyToTetrominoCommand rawkey, game) of
    (Just command, Just tetris) -> {game = Just (applyOnField (applyCommand command) tetris) }
    _ -> {game = game}

keyToTetrominoCommand : RawKey -> Maybe TetrominoCommand
keyToTetrominoCommand rawKey = case (Keyboard.anyKeyOriginal rawKey) of
    Just (Character char) -> case char of
        "w" -> Just (Move MoveLeft)
        "c" -> Just (Move MoveRight)
        "x" -> Just (Move MoveDown)
        "s" -> Just (Rotate RotateLeft)
        "d" -> Just (Rotate RotateRight)
        _ -> Nothing
    Just Spacebar -> Nothing
    _ -> Nothing

-- SUBSCRIPTIONS

subscriptions: Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs KeyDown ]

-- VIEW

buildRow: Row -> Html Msg
buildRow row =
    div [
        css [
              height (px 20)
            , margin (px 0)
        ]
    ]
     (List.map buildCell (Array.toList row))

cellColor : Cell -> Color
cellColor cell = case cell of
    Moving -> rgb 150 150 150
    Fixed -> rgb 0 0 0
    Empty -> rgb 255 255 255

buildCell: Cell -> Html Msg
buildCell cell =
        div [
            css
                [ display inlineBlock
                , width (px 20)
                , height (pct 100)
                , boxSizing borderBox
                , border3 (px 0.1) solid (rgb 0 0 0)
                , backgroundColor (cellColor cell)
                ]
        ]
        []

buildGrid : Grid ->  Html Msg
buildGrid grid =
  div []
  (List.map buildRow (Array.toList grid))


view : Model -> Html Msg
view {game} =
    case game of
        Nothing ->  div [] [ button [ onClick StartGame ] [ text "start game" ] ]
        Just tetris -> div [] [
                retrieveField tetris |> retrieveGrid |> buildGrid
                , button [ onClick StartGame ] [ text "restart game" ]
            ]