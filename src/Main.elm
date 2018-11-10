import Browser exposing (Document, document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Random
import Json.Decode as Decode

import GameModel exposing(..)
import Minefield exposing (Cell, Content(..), Minefield, isFresh, isMine)
import Cache

main =
    Browser.document
        { init = init
        , view = view
        , update = updateAndCache
        , subscriptions = subscriptions
        }

init : () -> ( GameModel, Cmd Msg )
init _ =
    ( GameModel Playing 37 <| Minefield.init 10, Cmd.none )


subscriptions : GameModel -> Sub Msg
subscriptions model =
    Cache.loadModel LoadFromCache
    -- Sub.none

-- Update

type Msg
    = ClickCell Cell
    | InitCell Cell Float
    | NewGame
    | Difficulty String
    | LoadFromCache (Result Decode.Error GameModel)

updateAndCache : Msg -> GameModel -> ( GameModel, Cmd Msg)
updateAndCache msg model =
    let
        (newModel, cmd) = update msg model
    in
        (newModel, Cmd.batch [cmd, Cache.cacheModel newModel] )

update : Msg -> GameModel -> ( GameModel, Cmd Msg )
update msg model =
    case msg of
        Difficulty difficulty ->
            ( { model | difficulty = Maybe.withDefault model.difficulty <| String.toInt difficulty }, Cmd.none )

        ClickCell cell ->
            let
                updCell =
                    updateCell cell

                minefield =
                    Minefield.replace updCell model.field

                cmd =
                    Minefield.rows minefield
                        |> List.foldl (++) []
                        |> List.filter isFresh
                        |> List.map (\c -> Random.generate (InitCell c) (Random.float 0 1))
                        |> Cmd.batch

                state =
                    if isMine cell then
                        GameOver

                    else
                        model.state
            in
                ( { model | field = minefield, state = state }, cmd )

        InitCell cell rnJesus ->
            let
                updCell =
                    { cell | content = Hidden <| rnJesus <= ( toFloat model.difficulty ) / 100  }
            in
                ( { model | field = Minefield.replace updCell model.field }, Cmd.none )

        NewGame ->
            ( { model | state = Playing, field = Minefield.init 10 }, Cmd.none )

        LoadFromCache result ->
            case result of
                Ok loadedModel ->
                    (loadedModel, Cmd.none)
                Err _ -> 
                    (model, Cmd.none)


updateCell cell =
    case cell.content of
        Fresh ->
            { cell | content = Visible False }

        Hidden hasMine ->
            { cell | content = Visible hasMine }

        Visible _ ->
            cell



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
        Playing ->
            div [] []

        GameOver ->
            h2 [] [ text "Game Over!" ]


viewField field =
    div [] (List.map (viewRow field) <| Minefield.rows field)


viewRow field row =
    div [ class "row" ] (List.map (viewCell field) row)


viewCell field cell =
    case cell.content of
        Visible hasMine ->
            if hasMine then
                div [ class "cell mine" ] []

            else
                div [ class "cell" ] [ text <| String.fromInt <| Minefield.adjacent cell field ]

        _ ->
            div [ class "cell hidden", onClick <| ClickCell cell ] []
