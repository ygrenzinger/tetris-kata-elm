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
import Playfield exposing (Cell(..), Grid, PlayField, PlayFieldState(..), Row, retrieveGrid)
import Random
import Shape exposing (Shape, allShapes, randomShapeGenerator)
import Tetris as T exposing (SpawnCommand(..), Tetris(..))
import Tetromino exposing (MoveCommand(..), RotateCommand(..), TetrominoCommand(..))
import Time



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view >> toUnstyled
        }



-- MODEL


type Game
    = NotStarted
    | Started Tetris
    | GameOver Tetris


type alias Model =
    Game


init : () -> ( Model, Cmd Msg )
init _ =
    ( NotStarted, Cmd.none )



-- UPDATE


type Msg
    = KeyDown RawKey
    | Tick Time.Posix
    | SpawnTetromino ( Maybe Shape, List Shape )
    | StartGame


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartGame ->
            ( Started <| T.startTetris, Random.generate SpawnTetromino (randomShapeGenerator allShapes) )

        _ ->
            case model of
                NotStarted ->
                    ( model, Cmd.none )

                GameOver _ ->
                    ( model, Cmd.none )

                Started tetris ->
                    case msg of
                        StartGame ->
                            ( Started <| T.startTetris, Random.generate SpawnTetromino (randomShapeGenerator allShapes) )

                        KeyDown rawKey ->
                            ( Started <| applyKeyPress tetris rawKey, Cmd.none )

                        Tick _ ->
                            Tuple.mapFirst Started <| applyGameLoop tetris

                        SpawnTetromino ( Nothing, _ ) ->
                            ( model, Random.generate SpawnTetromino (randomShapeGenerator allShapes) )

                        SpawnTetromino ( Just shape, availableShapes ) ->
                            spawnTetromino shape availableShapes tetris


spawnTetromino : Shape -> List Shape -> Tetris -> ( Model, Cmd Msg )
spawnTetromino shape availableShapes tetris =
    case T.spawnTetromino shape availableShapes tetris of
        ( updatedTetris, Full ) ->
            ( GameOver updatedTetris, Cmd.none )

        ( updatedTetris, Playable ) ->
            ( Started updatedTetris, Cmd.none )


applyGameLoop : Tetris -> ( Tetris, Cmd Msg )
applyGameLoop tetris =
    case T.makePieceFallDown tetris of
        ( updatedTetris, SpawnRandomShape availableShapes ) ->
            ( updatedTetris, Random.generate SpawnTetromino (randomShapeGenerator availableShapes) )

        ( updatedTetris, _ ) ->
            ( updatedTetris, Cmd.none )


applyKeyPress : Tetris -> RawKey -> Tetris
applyKeyPress tetris rawkey =
    case keyToTetrisCommand rawkey of
        Just command ->
            T.applyTetrominoCommand command tetris

        _ ->
            tetris


keyToTetrisCommand : RawKey -> Maybe TetrominoCommand
keyToTetrisCommand rawKey =
    case Keyboard.anyKeyOriginal rawKey of
        Just ArrowDown ->
            Just (Move MoveDown)

        Just ArrowLeft ->
            Just (Move MoveLeft)

        Just ArrowRight ->
            Just (Move MoveRight)

        Just ArrowUp ->
            Just (Rotate RotateRight)

        Just Alt ->
            Just (Rotate RotateLeft)

        Just Control ->
            Just (Rotate RotateRight)

        Just Spacebar ->
            Nothing

        _ ->
            Nothing



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Keyboard.downs KeyDown
        , Time.every 1000 Tick
        ]



-- VIEW


buildRow : Row -> Html Msg
buildRow row =
    div
        [ css
            [ height (px 20)
            , margin (px 0)
            ]
        ]
        (List.map buildCell (Array.toList row))


cellColor : Cell -> Color
cellColor cell =
    case cell of
        Moving ->
            rgb 150 150 150

        Fixed ->
            rgb 0 0 0

        Empty ->
            rgb 255 255 255


buildCell : Cell -> Html Msg
buildCell cell =
    div
        [ css
            [ display inlineBlock
            , width (px 20)
            , height (pct 100)
            , boxSizing borderBox
            , border3 (px 0.1) solid (rgb 0 0 0)
            , backgroundColor (cellColor cell)
            ]
        ]
        []


buildGrid : Grid -> Html Msg
buildGrid grid =
    div []
        (List.map buildRow (Array.toList grid))


view : Model -> Html Msg
view model =
    case model of
        NotStarted ->
            div [] [ button [ onClick StartGame ] [ text "start game" ] ]

        GameOver tetris ->
            div []
                [ T.retrieveField tetris |> retrieveGrid |> buildGrid
                , button [ onClick StartGame ] [ text "restart game" ]
                ]

        Started tetris ->
            div []
                [ T.retrieveField tetris |> retrieveGrid |> buildGrid
                , button [ onClick StartGame ] [ text "restart game" ]
                ]
