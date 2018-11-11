module Minefield exposing (Cell, Content(..), Minefield, init, rows, clickCell, Msg, update)

import Array exposing (Array)
import Maybe exposing (andThen)
import Json.Decode as Decode
import Random

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
    | Visible Bool Int

type Msg
    = ClickFreshCell Cell (List Cell) (Float, List Float)
    | ClickFirstCell Cell (List Cell) (List Float)
    | ClickHiddenCell Cell (List Cell) (List Float)

init dim =
    Array.initialize dim (Cell Fresh)
        |> Array.map (Array.initialize dim)


rows : Minefield -> List (List Cell)
rows field =
    Array.map Array.toList field
        |> Array.toList

clickCell : Cell -> Minefield -> Cmd Msg
clickCell cell field =
    let
        adjacentFresh = 
            adjacent cell field
                |> List.filter isFresh

        adjacentGenerator = Random.list (List.length adjacentFresh) (Random.float 0 1)
    in
    case cell.content of
        Visible _ _ ->
            Cmd.none
        
        Hidden _ ->
            Random.generate (ClickHiddenCell cell adjacentFresh) adjacentGenerator

        Fresh ->
            if  all isFresh field then
                Random.generate (ClickFirstCell cell adjacentFresh) adjacentGenerator
            else
                Random.pair (Random.float 0 1) adjacentGenerator
                    |> Random.generate (ClickFreshCell cell adjacentFresh)
                
            
update : Msg -> Minefield -> Minefield
update msg field =
    let
        (freshCells, adjacentRnJesus) =
            case msg of
                ClickFreshCell _ fresh (cellRnJesus, theAdjacentRnJesus) ->
                    (fresh, theAdjacentRnJesus)

                ClickFirstCell _ fresh theAdjacentRnJesus ->
                    (fresh, theAdjacentRnJesus)

                ClickHiddenCell _ fresh theAdjacentRnJesus ->
                    (fresh, theAdjacentRnJesus)

        (cell, cellContent) =
            case msg of
                ClickFreshCell clickedCell _ (cellRnJesus, _) ->
                    (clickedCell, Visible (cellRnJesus <= 0.37))

                ClickFirstCell clickedCell _ _ ->
                    (clickedCell, Visible False)

                ClickHiddenCell clickedCell _ _ ->
                    (clickedCell, Visible <| isMine clickedCell)
        
        updField = replaceAll field (List.map2 (\c f -> { c | content = Hidden (f <= 0.37) } ) freshCells adjacentRnJesus)
    in
    replace {cell | content = cellContent (adjacentMines cell updField) } updField


replaceAll : Minefield -> List Cell -> Minefield
replaceAll field cells =
    case List.head cells of
        Just cell ->
            replaceAll (replace cell field) (List.drop 1 cells)
        
        Nothing ->
            field

replace : Cell -> Minefield -> Minefield
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

        Visible hasMine _ ->
            hasMine

get field ( r, c ) =
    Array.get r field
        |> andThen (Array.get c)

adjacent : Cell -> Minefield -> List Cell
adjacent cell field =
    List.map (\( r, c ) -> ( cell.row + r, cell.col + c )) adjacentRel
        |> List.filterMap (get field)


adjacentMines : Cell -> Minefield -> Int
adjacentMines cell field =
    adjacent cell field
        |> List.filter isMine
        |> List.length

all : (Cell -> Bool) -> Minefield -> Bool
all predicate field =
    rows field
        |> List.concat
        |> List.all predicate