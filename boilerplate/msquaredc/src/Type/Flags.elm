module Type.Flags exposing (Flags)

import Json.Encode
import Type.LocalStorage



-- The expected flags on initialization, sent from JavaScript (static/index.js)


type alias Flags =
    { timeAppStarted : Int
    , windowSize : { width : Int, height : Int }
    , localStorage : Json.Encode.Value
    , db : Json.Encode.Value
    }
