module Main exposing (Msg(..), clickCell, init, initCmd, main, subscriptions, update, updateAndCache, view, viewCell, viewField, viewRow, viewState)

import Browser exposing (Document, document)
import Cache
import GameModel exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode
import Minefield exposing (Cell, Content(..), Minefield, isFresh)
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
    | InitCell Cell Float
    | NewGame
    | Difficulty String
    | LoadFromCache (Result Decode.Error GameModel)


updateAndCache : Msg -> GameModel -> ( GameModel, Cmd Msg )
updateAndCache msg model =
    let
        ( newModel, cmd ) =
            update msg model
    in
    ( newModel, Cmd.batch [ cmd, Cache.cacheModel newModel ] )


update : Msg -> GameModel -> ( GameModel, Cmd Msg )
update msg model =
    case msg of
        Difficulty difficulty ->
            ( { model | difficulty = Maybe.withDefault model.difficulty <| String.toInt difficulty }, Cmd.none )

        ClickCell cell ->
            let
                initAdjacent = 
                    Minefield.adjacent cell model.field
                        |> List.filter isFresh
                        |> List.map initCmd
                        |> Cmd.batch
            in
            case cell.content of
                Fresh ->
                    case model.state of
                        FreshGame ->
                            ( updateModel model { cell | content = Visible False}, initAdjacent )

                        _ ->
                            ( model, Cmd.batch [(initCmd { cell | content = Visible False }), initAdjacent] )

                Hidden hasMine ->
                    ( updateModel model { cell | content = Visible hasMine } , initAdjacent )

                Visible _ ->
                    ( model, Cmd.none )

        InitCell cell rnJesus ->
            let
                isMine =
                    rnJesus <= toFloat model.difficulty / 100
            in
            case cell.content of
                Visible _ ->
                    ( updateModel model { cell | content = Visible isMine } , Cmd.none)

                _ ->
                    ( updateModel model { cell | content = Hidden isMine } , Cmd.none )

        NewGame ->
            ( { model | state = FreshGame, field = Minefield.init 10 }, Cmd.none )

        LoadFromCache result ->
            case result of
                Ok loadedModel ->
                    ( loadedModel, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

updateModel : GameModel -> Cell -> GameModel
updateModel model cell =
    let 
        field = Minefield.replace cell model.field
        state =
            case cell.content of
                Visible True ->
                    GameOver
                
                _ -> 
                    model.state
    in
    {model | field = field, state = state}

-- Create command to fetch a random value and send an InitCell message
initCmd : Cell -> Cmd Msg
initCmd cell =
    Random.generate (InitCell cell) (Random.float 0 1)


clickCell : Cell -> Cell
clickCell cell =
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
        GameOver ->
            h2 [] [ text "Game Over!" ]

        _ ->
            div [] []


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
                div [ class "cell" ] [ text <| String.fromInt <| Minefield.adjacentMines cell field ]

        -- Hidden _ ->
        --     div [ class "cell hidden", onClick <| ClickCell cell ] [ text "H"]
            
        -- Fresh ->
        --     div [ class "cell hidden", onClick <| ClickCell cell ] [ text "F"]
        _ ->
            div [ class "cell hidden", onClick <| ClickCell cell ] []
