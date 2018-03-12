import Html exposing (..)
import Html.Attributes exposing (..)
import Random


main = Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


type alias Model =
    { table: List (List Cell)
    }


type Cell
    = Hidden Content
    | Visible Content

type Content
    = Mine
    | Safe

initCell : Random.Seed -> Float -> (Cell, Random.Seed)
initCell seed probability =
    let
        random = Random.step (Random.float 0 1) seed
        content = if Tuple.first random <= probability then
                Mine
            else
                Safe
    in (Hidden content, Tuple.second random)

initCol : Random.Seed -> Float -> Int -> (List Cell, Random.Seed)
initCol seed probability height =
    if height == 0 then
        ([], seed)
    else
        let
            cell = initCell seed probability
            rest = initCol (Tuple.second cell) probability (height-1)
        in
            (Tuple.first cell :: (Tuple.first rest), Tuple.second rest)

initTable : Random.Seed -> Float -> Int -> Int -> (List (List Cell), Random.Seed)
initTable seed probability width height =
    if width == 0 then
        ([], seed)
    else
        let
            col = initCol seed probability height
            rest = initTable (Tuple.second col) probability (width-1) height
        in
            ((Tuple.first col) :: (Tuple.first rest), Tuple.second rest)


init : (Model, Cmd Msg)
init =
    let
        table = initTable (Random.initialSeed 1) 0.34 5 5
    in
        (Model (Tuple.first table), Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- Update

type Msg = Something

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    (model, Cmd.none)


-- View
view : Model -> Html Msg
view model =
    div []
        [ h2 [] [text "hello world" ]
        , viewBoard model.table
        ]


viewBoard : List (List Cell) -> Html Msg
viewBoard table =
    div [id "game-board"] (List.map viewRow table)

viewRow : List Cell -> Html Msg
viewRow row =
    div [ class "row" ] (List.map viewCell row)


viewCell : Cell -> Html Msg
viewCell cell =
    case cell of
        Hidden content ->
            div [ class "hidden" ] [text ("h." ++ (toString content))]

        Visible content ->
            div [ class "visible" ] [text ("v." ++ (toString content))]

