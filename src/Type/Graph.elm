module Type.Graph exposing (..)

import Dict exposing (Dict)
import Json.Decode

type alias Decoder container decoded
    = {
        container | decode : Json.Decode.Decoder decoded
    }
            
type alias DataView container source mediator target
    = {
        container | view : (source -> mediator -> target)
    }