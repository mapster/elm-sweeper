module GameModel exposing (..)

import Minefield exposing (Minefield, Cell)

type alias GameModel =
    { mouse : MouseState
    , state : GameState
    , difficulty : Int
    , field : Minefield
    }

type GameState
    = Playing
    | GameOver
    | FreshGame

type alias MouseState =
    { clicks: Maybe (Cell, Maybe Cell)
    , count: Int
    }

initGame : GameModel
initGame = 
    GameModel initMouse FreshGame 37 (Minefield.init 10 )

initMouse : MouseState
initMouse = 
    MouseState Nothing 0

mouseDown : Cell -> MouseState -> MouseState
mouseDown cell mouse =
    { mouse 
    | clicks = Just (cell, Nothing)
    , count = 0
    }

mouseUp : Cell -> MouseState -> MouseState
mouseUp cell mouse =
    case mouse.clicks of
        Just ( down, Nothing ) ->
            { mouse | clicks = Just (down, Just cell) }

        Just ( down, Just _ )  ->
            { mouse | clicks = Nothing }

        Nothing ->
            mouse

mouseTick : MouseState -> MouseState
mouseTick mouse =
    case mouse.clicks of
        Nothing ->
            initMouse
        
        Just ( _, Nothing ) ->
            { mouse | count = mouse.count + 1 }

        Just ( _, Just _ ) ->
            mouse

isClick : MouseState -> Bool
isClick mouse =
    case mouse.clicks of
        Just (down, Just up) ->
            if ( (==) down up ) && ( mouse.count <= 1 ) then
                True
            else
                False
        
        _ ->
            False

isLongClick : MouseState -> Bool
isLongClick mouse =
    case mouse.clicks of
        Just (down, Just up) ->
            if ( (==) down up ) && ( mouse.count > 1 ) then
                True
            else
                False
        
        _ ->
            False