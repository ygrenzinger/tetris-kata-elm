module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Array
import Browser exposing (Document)
import Css exposing (..)
import Html.Styled exposing (Attribute, Html, button, div, header, nav, span, styled, text, toUnstyled)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Keyboard exposing (Key(..), KeyChange(..), RawKey)
import Playfield exposing (Cell(..), Grid, PlayField, PlayFieldState(..), Row, retrieveGrid)
import Random
import Shape exposing (Shape, TetrominoShape, allShapes, randomShapeGenerator)
import Tetris as T exposing (ScoringSystem, SpawnCommand(..), Tetris(..), levelToString, retrieveScore, scoreToString, timeSpentInRow)
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
        (List.map displayCell (Array.toList row))


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


displayCell : Cell -> Html Msg
displayCell cell =
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


displayGrid : Grid -> Html Msg
displayGrid grid =
    div
        [ css
            [ boxShadow5 (px 3) (px 4) (px 0) (px 0) (hex "899599")
            , backgroundColor (hex "ededed")
            , border3 (px 1) solid (hex "d6bcd6")
            ]
        ]
        (List.map buildRow (Array.toList grid |> List.drop 2))


displayScore : ScoringSystem -> Html Msg
displayScore scoring =
    div []
        [ div
            [ css
                [ padding2 (px 10) (px 0)
                ]
            ]
            [ text ("Level " ++ levelToString scoring) ]
        , div
            [ css
                [ padding2 (px 5) (px 0)
                ]
            ]
            [ text ("Score " ++ scoreToString scoring) ]
        ]


buildTetris : String -> Tetris -> Html Msg
buildTetris title tetris =
    div
        [ css
            [ width (px 500)
            , displayFlex
            , justifyContent spaceAround
            ]
        ]
        [ div
            []
            [ T.retrieveField tetris |> retrieveGrid |> displayGrid ]
        , div
            [ css
                [ textAlign center
                ]
            ]
            [ div
                [ css
                    [ fontSize (px 20)
                    , fontWeight bold
                    , padding2 (px 5) (px 0)
                    ]
                ]
                [ text title ]
            , div [] [ displayScore (retrieveScore tetris) ]
            , btn [ onClick StartGame ] [ text "restart game" ]
            ]
        ]


buildGame : Model -> Html Msg
buildGame model =
    case model of
        NotStarted ->
            btn [ onClick StartGame ] [ text "start game" ]

        GameOver tetris ->
            buildTetris "Game Over" tetris

        Playing tetris ->
            buildTetris "Running" tetris


buildPage : Model -> Html Msg
buildPage model =
    div
        [ css
            [ paddingTop (px 10)
            , displayFlex
            , justifyContent center
            , alignItems center
            ]
        ]
        [ buildGame model
        ]


view : Model -> Document Msg
view model =
    { title = "Tetris Kata in Elm"
    , body = List.singleton (buildPage model |> toUnstyled)
    }


btn : List (Attribute msg) -> List (Html msg) -> Html msg
btn =
    styled button
        [ display inlineBlock
        , color (hex "ffffff")
        , backgroundColor (hex "44c767")
        , padding2 (px 15) (px 30)
        , border3 (px 1) solid (hex "18ab29")
        , borderRadius (px 28)
        , cursor pointer
        , fontFamilies [ "Arial" ]
        , fontSize (px 16)
        , textDecoration none
        , textShadow4 (px 0) (px 1) (px 0) (hex "2f6627")
        , hover
            [ backgroundColor (hex "5cbf2a")
            ]
        , active
            [ position relative
            , top (px 1)
            ]
        ]
