module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--


import Browser
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (height, style, width)
import Html.Events exposing (onClick)



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL



type Cell = Empty | Full String

type alias Row = List Cell

type alias Grid = List Row


type alias Model = Grid

initRow : Row
initRow = List.map (\_ -> Empty) (List.range 1 10)

switchCellToFull : Cell
switchCellToFull = Full "red"

switchRowToFull : Row -> Row
switchRowToFull row = List.map (\_ -> switchCellToFull) row

switchGridToFull : Grid -> Grid
switchGridToFull grid =  List.map switchRowToFull grid

init : Model
init =
    List.map (\_ -> initRow) (List.range 1 24)

-- UPDATE


type Msg
  = ChangeColor


update : Msg -> Model -> Model
update msg model =
  case msg of
      ChangeColor -> switchGridToFull model

-- VIEW

buildRow: Row -> Html Msg
buildRow row =
    div []
     (List.map buildCell row)

colorOfCell : Cell -> String
colorOfCell cell =
    case cell of
        Empty -> "black"
        (Full color) -> color

buildCell: Cell -> Html Msg
buildCell cell =
        div [
            style "display" "inline-block",
            style "background-color" (colorOfCell cell),
            style "margin" "1px",
            style "width" "10px",
            style "height" "10px"
        ] []

buildGrid : Grid ->  Html Msg
buildGrid grid =
  div []
    (List.map buildRow grid)


view : Model -> Html Msg
view model =
  div []
    [
    (buildGrid model)

    , button [ onClick ChangeColor ] [ text "switch to full" ]
    ]