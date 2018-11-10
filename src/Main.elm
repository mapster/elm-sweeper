import Browser exposing (Document, document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Minefield exposing (Cell, Content(..), Minefield, isFresh, isMine)
import Random


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { state : GameState
    , difficulty : Int
    , field : Minefield
    }


type GameState
    = Playing
    | GameOver


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Playing 37 <| Minefield.init 10, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- Update


type Msg
    = ClickCell Cell
    | InitCell Cell Float
    | NewGame
    | Difficulty String


update : Msg -> Model -> ( Model, Cmd Msg )
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


updateCell cell =
    case cell.content of
        Fresh ->
            { cell | content = Visible False }

        Hidden hasMine ->
            { cell | content = Visible hasMine }

        Visible _ ->
            cell



-- View


view : Model -> Browser.Document Msg
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
