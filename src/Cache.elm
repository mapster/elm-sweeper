port module Cache exposing (cacheModel, loadModel)

import Array exposing (Array)
import Json.Encode as Encode
import Json.Decode as Decode

import GameModel exposing (..)
import Minefield exposing (Minefield, Cell, Content(..))

cacheModel : GameModel -> Cmd msg
cacheModel model =
    cache <| encodeModel model

loadModel : (Result Decode.Error GameModel -> msg) -> Sub msg
loadModel msg =
    loadCache (\val -> msg <| Decode.decodeValue modelDecoder val)

port cache : Encode.Value -> Cmd msg
port loadCache : (Encode.Value -> msg) -> Sub msg

-- Main Model

encodeModel : GameModel -> Encode.Value
encodeModel model =
    Encode.object
        [ ("state", encodeState model.state)
        , ("difficulty", Encode.int model.difficulty)
        , ("field", encodeMinefield model.field)
        ]

encodeState : GameState -> Encode.Value
encodeState state =
    case state of
        Playing -> 
            Encode.string "Playing"
        GameOver ->
            Encode.string "GameOver"

modelDecoder : Decode.Decoder GameModel
modelDecoder =
    Decode.map3 GameModel
        (Decode.field "state" stateDecoder)
        (Decode.field "difficulty" Decode.int)
        (Decode.field "field" minefieldDecoder)

stateDecoder : Decode.Decoder GameState
stateDecoder =
    Decode.string
        |> Decode.andThen (\str ->
            case str of
                "Playing" ->
                    Decode.succeed Playing
                "GameOver" ->
                    Decode.succeed GameOver
                other ->
                    Decode.fail <| "Unknown state: " ++ other
        )

-- Minefield

encodeMinefield : Minefield -> Encode.Value
encodeMinefield field =
    Encode.array encodeRow field

encodeRow : (Array Cell) -> Encode.Value
encodeRow row =
    Encode.array encodeCell row

encodeCell : Cell -> Encode.Value
encodeCell cell =
    Encode.object
        [ ("row", Encode.int cell.row)
        , ("col", Encode.int cell.col)
        , ("content", encodeContent cell.content)
        ]

encodeContent : Content -> Encode.Value
encodeContent content =
    case content of
        Fresh ->
            Encode.string "Fresh"
        
        Hidden hasMine ->
            Encode.object
                [ ("visible", Encode.bool False)
                , ("hasMine", Encode.bool hasMine)
                ]
        
        Visible hasMine ->
            Encode.object
                [ ("visible", Encode.bool True)
                , ("hasMine", Encode.bool hasMine)
                ]

minefieldDecoder : Decode.Decoder Minefield
minefieldDecoder =
    Decode.array rowDecoder

rowDecoder : Decode.Decoder (Array Cell)
rowDecoder =
    Decode.array cellDecoder

cellDecoder : Decode.Decoder Cell
cellDecoder =
    Decode.map3 Cell
        (Decode.field "content" contentDecoder)
        (Decode.field "row" Decode.int)
        (Decode.field "col" Decode.int)

contentDecoder : Decode.Decoder Content
contentDecoder =
    Decode.oneOf [freshDecoder, nonFreshDecoder]

freshDecoder : Decode.Decoder Content
freshDecoder =
    Decode.string
        |> Decode.andThen (\str -> 
            case str of
                "Fresh" ->
                    Decode.succeed Fresh
                
                other ->
                    Decode.fail <| "Unknown state: " ++ str
        )

nonFreshDecoder : Decode.Decoder Content
nonFreshDecoder =
    Decode.map2 contentConstruct
        (Decode.field "visible" Decode.bool)
        (Decode.field "hasMine" Decode.bool)

contentConstruct : Bool -> Bool -> Content
contentConstruct visible hasMine =
    if visible then
        Visible hasMine
    else         
        Hidden hasMine