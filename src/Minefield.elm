module Minefield exposing (Cell, Content(..), Minefield, init, rows, toggleFlag, clickCell, Msg, update)

import Array exposing (Array)
import Maybe exposing (andThen)
import Json.Decode as Decode
import Random

adjacentRel =
    [ ( -1, -1 ), ( -1, 0 ), ( -1, 1 ), ( 0, -1 ), ( 0, 1 ), ( 1, -1 ), ( 1, 0 ), ( 1, 1 ) ]

type alias Minefield =
    Array (Array Cell)

type alias MineGenerator =
    Random.Generator Bool

type alias Cell =
    { content : Content
    , row : Int
    , col : Int
    }


type Content
    = Fresh
    | Hidden Bool
    | Visible Bool Int
    | Flag Content

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

toggleFlag : Minefield -> Cell -> Minefield
toggleFlag field cell =
    case cell.content of
        Flag content ->
            replace { cell | content = content } field

        _ ->
            replace { cell | content = Flag cell.content } field

clickCell : MineGenerator -> Minefield -> Cell -> Cmd Msg
clickCell mineGenerator field cell =
    let
        adjacentFresh = 
            adjacent field cell
                |> List.filter isFresh

        adjacentGenerator = Random.list (List.length adjacentFresh) mineGenerator
    in
    case cell.content of        
        Hidden _ ->
            Random.generate (ClickHiddenCell cell adjacentFresh) adjacentGenerator

        Fresh ->
            if  all isFresh field then
                Random.generate (ClickFirstCell cell adjacentFresh) adjacentGenerator
            else
                Random.pair mineGenerator adjacentGenerator
                    |> Random.generate (ClickFreshCell cell adjacentFresh)

        _ ->
            Cmd.none

update2 : (Minefield -> Cell) -> List Cell -> List Bool -> Minefield -> Minefield
update2 updateCell freshCells freshCellsRnJesus field =
    let
        updField = 
            List.map2 asHidden freshCells freshCellsRnJesus
                |> replaceAll field
    in
    replace (updateCell updField) updField

update : MineGenerator -> Msg -> Minefield -> (Minefield, Cmd Msg)
update mineGenerator msg field =
    let
        updField =
            case msg of
                ClickFirstCell clickedCell freshCells freshCellsRnJesus ->
                    update2 ( asVisible clickedCell False) freshCells freshCellsRnJesus field

                ClickFreshCell clickedCell freshCells ( clickedRnJesus, freshCellsRnJesus ) ->
                    update2 ( asVisible clickedCell clickedRnJesus ) freshCells freshCellsRnJesus field

                ClickHiddenCell clickedCell freshCells freshCellsRnJesus ->
                    update2 ( asVisible clickedCell ( isMine clickedCell ) ) freshCells freshCellsRnJesus field
        
        clickCmd =
            filter isZero updField
                |> List.map (adjacent updField)
                |> List.concat
                |> List.filter isHidden
                |> List.head
                |> Maybe.map (clickCell mineGenerator updField)
                |> Maybe.withDefault Cmd.none
    in
    (updField, clickCmd)

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

isFresh : Cell -> Bool
isFresh cell =
    case cell.content of
        Fresh ->
            True

        _ ->
            False

isMine : Cell -> Bool
isMine cell =
    case cell.content of
        Fresh ->
            False

        Hidden hasMine ->
            hasMine

        Visible hasMine _ ->
            hasMine

        Flag content ->
            isMine { cell | content = content }

isHidden : Cell -> Bool
isHidden cell =
    case cell.content of
        Hidden _ ->
            True
        
        _ -> 
            False

isZero : Cell -> Bool
isZero cell =
    case cell.content of
        Visible _ 0 ->
            True

        _ ->
            False

get field ( r, c ) =
    Array.get r field
        |> andThen (Array.get c)

adjacent : Minefield -> Cell -> List Cell
adjacent field cell =
    List.map (\( r, c ) -> ( cell.row + r, cell.col + c )) adjacentRel
        |> List.filterMap (get field)


adjacentMines : Cell -> Minefield -> Int
adjacentMines cell field =
    adjacent field cell 
        |> List.filter isMine
        |> List.length

all : (Cell -> Bool) -> Minefield -> Bool
all predicate field =
    rows field
        |> List.concat
        |> List.all predicate

filter : (Cell -> Bool) -> Minefield -> List Cell
filter predicate field =
    rows field
        |> List.concat
        |> List.filter predicate