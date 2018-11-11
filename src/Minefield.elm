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
    = ClickFreshCell Cell (List Cell) (Bool, List Bool)
    | ClickFirstCell Cell (List Cell) (List Bool)
    | ClickHiddenCell Cell (List Cell) (List Bool)

init dim =
    Array.initialize dim (Cell Fresh)
        |> Array.map (Array.initialize dim)


rows : Minefield -> List (List Cell)
rows field =
    Array.map Array.toList field
        |> Array.toList

clickCell : Random.Generator Bool -> Cell -> Minefield -> Cmd Msg
clickCell mineGenerator cell field =
    let
        adjacentFresh = 
            adjacent cell field
                |> List.filter isFresh

        adjacentGenerator = Random.list (List.length adjacentFresh) mineGenerator
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
                Random.pair mineGenerator adjacentGenerator
                    |> Random.generate (ClickFreshCell cell adjacentFresh)

update2 : (Minefield -> Cell) -> List Cell -> List Bool -> Minefield -> Minefield
update2 updateCell freshCells freshCellsRnJesus field =
    let
        updField = 
            List.map2 asHidden freshCells freshCellsRnJesus
                |> replaceAll field
    in
        replace (updateCell updField) updField

update : Msg -> Minefield -> Minefield
update msg field =
    case msg of
        ClickFirstCell clickedCell freshCells freshCellsRnJesus ->
            update2 ( asVisible clickedCell False) freshCells freshCellsRnJesus field

        ClickFreshCell clickedCell freshCells ( clickedRnJesus, freshCellsRnJesus ) ->
            update2 ( asVisible clickedCell clickedRnJesus ) freshCells freshCellsRnJesus field

        ClickHiddenCell clickedCell freshCells freshCellsRnJesus ->
            update2 ( asVisible clickedCell ( isMine clickedCell ) ) freshCells freshCellsRnJesus field

asHidden : Cell -> Bool -> Cell
asHidden cell hasMine =
    { cell | content = Hidden hasMine }

asVisible : Cell -> Bool -> Minefield -> Cell
asVisible cell hasMine field =
    { cell | content = Visible hasMine (adjacentMines cell field) } 

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