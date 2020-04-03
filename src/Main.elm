module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Array
import Browser exposing (Document)
import Css exposing (..)
import Html.Styled exposing (Html, button, div, header, nav, text, toUnstyled)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Keyboard exposing (Key(..), KeyChange(..), RawKey)
import Playfield exposing (Cell(..), Grid, PlayField, PlayFieldState(..), Row, retrieveGrid)
import Random
import Shape exposing (Shape, TetrominoShape, allShapes, randomShapeGenerator)
import Tetris as T exposing (SpawnCommand(..), Tetris(..), levelToString, scoreToString, timeSpentInRow)
import Tetromino exposing (MoveCommand(..), RotateCommand(..), TetrominoCommand(..))
import Time



-- MAIN


main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type Game
    = NotStarted
    | Playing Tetris
    | GameOver Tetris


type alias Model =
    Game


init : () -> ( Model, Cmd Msg )
init _ =
    ( NotStarted, Cmd.none )



-- UPDATE


type Msg
    = KeyDown RawKey
    | Tick
    | SpawnTetromino ( Maybe TetrominoShape, List TetrominoShape )
    | StartGame


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartGame ->
            ( Playing <| T.startTetris, Random.generate SpawnTetromino (randomShapeGenerator allShapes) )

        _ ->
            case model of
                NotStarted ->
                    ( model, Cmd.none )

                GameOver _ ->
                    ( model, Cmd.none )

                Playing tetris ->
                    case msg of
                        StartGame ->
                            ( Playing <| T.startTetris, Random.generate SpawnTetromino (randomShapeGenerator allShapes) )

                        KeyDown rawKey ->
                            ( Playing <| applyKeyPress tetris rawKey, Cmd.none )

                        Tick ->
                            Tuple.mapFirst Playing <| applyGameLoop tetris

                        SpawnTetromino ( Nothing, _ ) ->
                            ( model, Random.generate SpawnTetromino (randomShapeGenerator allShapes) )

                        SpawnTetromino ( Just shape, availableShapes ) ->
                            spawnTetromino shape availableShapes tetris


spawnTetromino : TetrominoShape -> List TetrominoShape -> Tetris -> ( Model, Cmd Msg )
spawnTetromino shape availableShapes tetris =
    case T.spawnTetromino shape availableShapes tetris of
        ( updatedTetris, Full ) ->
            ( GameOver updatedTetris, Cmd.none )

        ( updatedTetris, Playable ) ->
            ( Playing updatedTetris, Cmd.none )


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
            Just Drop

        _ ->
            Nothing



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Playing tetris ->
            Sub.batch
                [ Keyboard.downs KeyDown
                , Time.every (timeSpentInRow tetris) (\_ -> Tick)
                ]

        _ ->
            Sub.none



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
        Moving color ->
            hex color

        Fixed color ->
            hex color

        Empty ->
            hex "ffffff"


cellBorder : Cell -> Style
cellBorder cell =
    case cell of
        Moving color ->
            border3 (px 0.1) solid (hex color)

        _ ->
            border3 (px 0.1) solid (rgb 0 0 0)


buildCell : Cell -> Html Msg
buildCell cell =
    div
        [ css
            [ display inlineBlock
            , width (px 20)
            , height (pct 100)
            , boxSizing borderBox
            , cellBorder cell
            , backgroundColor (cellColor cell)
            ]
        ]
        []


buildGrid : Grid -> Html Msg
buildGrid grid =
    div []
        (List.map buildRow (Array.toList grid |> List.drop 2))


buildGame : Model -> Html Msg
buildGame model =
    case model of
        NotStarted ->
            nav [] [ button [ onClick StartGame ] [ text "start game" ] ]

        GameOver tetris ->
            div []
                [ header []
                    [ div [] [ T.retrieveField tetris |> retrieveGrid |> buildGrid ]
                    , div [] [ text "game over" ]
                    , div [] [ text ("Level " ++ scoreToString tetris) ]
                    , div [] [ text ("Score " ++ levelToString tetris) ]
                    , div [] [ button [ onClick StartGame ] [ text "restart game" ] ]
                    ]
                ]

        Playing tetris ->
            div []
                [ header []
                    [ div [] [ T.retrieveField tetris |> retrieveGrid |> buildGrid ]
                    , div [] [ text "playing" ]
                    , div [] [ text ("Level " ++ scoreToString tetris) ]
                    , div [] [ text ("Score " ++ levelToString tetris) ]
                    , div [] [ button [ onClick StartGame ] [ text "restart game" ] ]
                    ]
                ]


view : Model -> Document Msg
view model =
    { title = "Tetris Kata in Elm"
    , body = List.singleton (buildGame model |> toUnstyled)
    }
