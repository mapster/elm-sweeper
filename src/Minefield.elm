module Minefield exposing (Cell, Content(..), Minefield, adjacent, adjacentRel, get, init, isFresh, isMine, replace, rows)

import Array exposing (Array)
import Maybe exposing (andThen)
import Json.Decode as Decode

adjacentRel =
    [ ( -1, -1 ), ( -1, 0 ), ( -1, 1 ), ( 0, -1 ), ( 0, 1 ), ( 1, -1 ), ( 1, 0 ), ( 1, 1 ) ]


type alias Minefield =
    Array (Array Cell)


type alias Cell =
    { content : Content
    , row : Int
    , col : Int
    }


type Content
    = Fresh
    | Hidden Bool
    | Visible Bool

init dim =
    Array.initialize dim (Cell Fresh)
        |> Array.map (Array.initialize dim)


rows field =
    Array.map Array.toList field
        |> Array.toList


replace cell field =
    case Array.get cell.row field of
        Just row ->
            let
                updRow =
                    Array.set cell.col cell row
            in
            Array.set cell.row updRow field

        Nothing ->
            field


isFresh cell =
    case cell.content of
        Fresh ->
            True

        _ ->
            False


isMine cell =
    case cell.content of
        Fresh ->
            False

        Hidden hasMine ->
            hasMine

        Visible hasMine ->
            hasMine


get field ( r, c ) =
    Array.get r field
        |> andThen (Array.get c)


adjacent cell field =
    List.map (\( r, c ) -> ( cell.row + r, cell.col + c )) adjacentRel
        |> List.filterMap (get field)
        |> List.filter isMine
        |> List.length
