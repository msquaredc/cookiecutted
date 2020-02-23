module Type.LocalStorage exposing (LocalStorage, decode, encode)

import Json.Decode
import Json.Decode.Pipeline
import Json.Encode



-- This is the type for what you'd like to store in local storage. It can be whatever you'd like
-- Typically, it will be some sort of credentials, i.e. a token


type alias LocalStorage =
    { token : String
    }



-- Decoder & Encoder for the LocalStorage type.
-- I recommend checking https://noredink.github.io/json-to-elm/ for generating decoders/encoders for more complex types


decode : Json.Decode.Decoder (Maybe LocalStorage)
decode =
    Json.Decode.nullable <|
        (Json.Decode.succeed LocalStorage
            |> Json.Decode.Pipeline.required "token" Json.Decode.string
        )


encode : LocalStorage -> Json.Encode.Value
encode record =
    Json.Encode.object [ ( "token", Json.Encode.string record.token ) ]
