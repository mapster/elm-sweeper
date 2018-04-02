import Html exposing (..)
import Html.Attributes exposing (..)
import Minefield
import Random


main = Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


type alias Model =
    {
    }

init : (Model, Cmd Msg)
init =
    (Model, Cmd.none)

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
        [
        ]
