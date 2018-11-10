module GameModel exposing (..)

import Minefield exposing (Minefield)

type alias GameModel =
    { state : GameState
    , difficulty : Int
    , field : Minefield
    }

type GameState
    = Playing
    | GameOver