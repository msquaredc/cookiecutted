module Type.AnyDict exposing (..)

import Dict exposing (Dict)
import Type.AnyDict.Int as AnyInt

type AnyDict k v =
    AnyDict {
        int : Dict k Int
    }

empty : AnyDict k v 
empty = 
    AnyDict {
        int = Dict.empty
    }

insert : comparable -> v -> 