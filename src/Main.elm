import Browser exposing (Document, document)
import Cache
import GameModel exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode
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
    ( GameModel FreshGame 37 <| Minefield.init 10, Cmd.none )


subscriptions : GameModel -> Sub Msg
subscriptions model =
    Cache.loadModel LoadFromCache


-- Update

type Msg
    = ClickCell Cell
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

mineGenerator : Int -> Random.Generator Bool
mineGenerator difficulty =
    Random.weighted (toFloat difficulty, True) [ ( toFloat <| 100 - difficulty, False) ]

update : Msg -> GameModel -> ( GameModel, Cmd Msg )
update msg model =
    case msg of
        Difficulty difficulty ->
            ( { model | difficulty = Maybe.withDefault model.difficulty <| String.toInt difficulty }, Cmd.none )

        ClickCell cell ->
            ( model
            , Minefield.clickCell (mineGenerator model.difficulty) cell model.field
                |> Cmd.map MinefieldMsg 
            )

        NewGame ->
            ( { model | state = FreshGame, field = Minefield.init 10 }, Cmd.none )

        LoadFromCache result ->
            case result of
                Ok loadedModel ->
                    ( loadedModel, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )
            
        MinefieldMsg fieldMsg ->
            ( { model | field = Minefield.update fieldMsg model.field }, Cmd.none)

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
            ]
        ]
    }


viewState state =
    case state of
        GameOver ->
            h2 [] [ text "Game Over!" ]

        _ ->
            div [] []


viewField field =
    div [] (List.map viewRow <| Minefield.rows field)


viewRow row =
    div [ class "row" ] (List.map viewCell row)


viewCell cell =
    case cell.content of
        Visible hasMine adjacentMines ->
            if hasMine then
                div [ class "cell mine" ] []

            else
                div [ class "cell" ] [ text <| String.fromInt adjacentMines ]

        Hidden hasMine ->
            let
                str = "H" ++ if hasMine then "M" else ""
            in    
            div [ class "cell hidden", onClick <| ClickCell cell ] [ text str]
            
        Fresh ->
            div [ class "cell hidden", onClick <| ClickCell cell ] [ text "F"]
        -- _ ->
        --     div [ class "cell hidden", onClick <| ClickCell cell ] []
