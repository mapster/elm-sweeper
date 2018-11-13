import Browser exposing (Document, document)
import Cache
import GameModel exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onMouseDown, onMouseUp, onClick, onInput)
import Json.Decode as Decode
import Time
import Minefield exposing (Cell, Content(..), Minefield)
import Random


main =
    Browser.document
        { init = init
        , view = view
        , update = updateAndCache
        , subscriptions = subscriptions
        }


init : () -> ( GameModel, Cmd Msg )
init _ =
    ( initGame, Cmd.none )


subscriptions : GameModel -> Sub Msg
subscriptions model =
    Sub.batch <|
        [ Cache.loadModel LoadFromCache
        , Time.every 200 MouseTick
        ]


-- Update

type Msg
    = MouseDown Cell
    | MouseUp Cell
    | MouseTick Time.Posix
    | NewGame
    | Difficulty String
    | LoadFromCache (Result Decode.Error GameModel)
    | MinefieldMsg Minefield.Msg


updateAndCache : Msg -> GameModel -> ( GameModel, Cmd Msg )
updateAndCache msg model =
    let
        ( newModel, cmd ) =
            update msg model
    in
    ( newModel, Cmd.batch [ cmd, Cache.cacheModel newModel ] )

mineGenerator : GameModel -> Random.Generator Bool
mineGenerator { difficulty} =
    Random.weighted (toFloat difficulty, True) [ ( toFloat <| 100 - difficulty, False) ]

update : Msg -> GameModel -> ( GameModel, Cmd Msg )
update msg model =
    case msg of
        MouseDown cell ->
            ({ model | mouse = mouseDown cell model.mouse }, Cmd.none)

        MouseUp cell ->
            let
                mouse = mouseUp cell model.mouse
            in
            if isClick mouse then                   
                ( { model | mouse = initMouse }
                , Minefield.clickCell (mineGenerator model) model.field cell
                    |> Cmd.map MinefieldMsg 
                )

            else if isLongClick mouse then
                ( { model | mouse = initMouse, field = Minefield.toggleFlag model.field cell }, Cmd.none )

            else
                ( { model | mouse = initMouse }, Cmd.none)

        MouseTick _ ->
            ( { model | mouse = mouseTick model.mouse}, Cmd.none )

        Difficulty difficulty ->
            ( { model | difficulty = Maybe.withDefault model.difficulty <| String.toInt difficulty }, Cmd.none )

        NewGame ->
            ( { model | state = FreshGame, field = Minefield.init 10 }, Cmd.none )

        LoadFromCache result ->
            case result of
                Ok loadedModel ->
                    ( loadedModel, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )
            
        MinefieldMsg fieldMsg ->
            let
                (field, cmd) = Minefield.update (mineGenerator model) fieldMsg model.field
            in
            ( { model | field = field }, Cmd.map MinefieldMsg cmd)


-- View


view : GameModel -> Browser.Document Msg
view model =
    { title = "Elm-sweeper"
    , body =
        [ div [ class "app" ]
            [ h1 [] [ text "Elm-sweeper" ]
            , div []
                [ text "Difficulty: "
                , input [ type_ "range", Html.Attributes.min "0", Html.Attributes.max "100", value <| String.fromInt model.difficulty, onInput Difficulty ] []
                , text <| String.fromInt <| model.difficulty
                ]
            , button [ onClick NewGame ] [ text "New Game" ]
            , viewField model.field
            , viewState model.state
            , viewMouse model.mouse
            ]
        ]
    }


viewState : GameState -> Html Msg
viewState state =
    case state of
        GameOver ->
            h2 [] [ text "Game Over!" ]

        _ ->
            div [] []


viewField : Minefield -> Html Msg
viewField field =
    div [] (List.map viewRow <| Minefield.rows field)


viewRow : List Cell -> Html Msg
viewRow row =
    div [ class "row" ] (List.map viewCell row)

viewCell : Cell -> Html Msg
viewCell cell =
    case cell.content of
        Visible True _ ->
            div [ class "cell mine" ] []

        Visible _ adjacentMines -> 
            div [ class "cell" ] [ text <| String.fromInt adjacentMines ]

        Flag _ ->
            div [ class "cell hidden", onMouseDown ( MouseDown cell ), onMouseUp ( MouseUp cell) ] [ text "F"]
        -- Hidden hasMine ->
        --     let
        --         str = "H" ++ if hasMine then "M" else ""
        --     in    
        --     div [ class "cell hidden", onClick <| OpenCell cell ] [ text str]
            
        -- Fresh ->
        --     div [ class "cell hidden", onClick <| OpenCell cell ] [ text "F"]
        _ ->
            div [ class "cell hidden", onMouseDown ( MouseDown cell ), onMouseUp ( MouseUp cell) ] []

viewMouse : MouseState -> Html Msg
viewMouse mouse =
    div [] 
        [ span [] [ text <| clicksCoord mouse ]
        , span [] [ text <| " count: " ++ (String.fromInt mouse.count) ]
        ]

clicksCoord : MouseState -> String
clicksCoord mouse =
    case mouse.clicks of
            Just ( down, Just up ) ->
                "down: " ++ (cellCoord down) ++ " up: " ++ (cellCoord up)

            Just ( down, Nothing ) ->
                "down: " ++ (cellCoord down) ++ " up: "

            Nothing ->
                ""
cellCoord : Cell -> String
cellCoord cell =
    "(" ++ (String.fromInt cell.row) ++ ", " ++ (String.fromInt cell.col) ++ ")"