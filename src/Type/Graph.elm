module Type.Graph exposing (Decoder)

import Json.Decode


type alias Decoder container decoded =
    { container
        | decode : Json.Decode.Decoder decoded
    }
