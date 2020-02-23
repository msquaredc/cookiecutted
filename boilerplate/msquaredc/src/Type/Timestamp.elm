module Type.Timestamp exposing (Timestamp, Msg(..))

import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode exposing (Value)
import Type.IO exposing (..)
import Time
import Fuzz

type alias Timestamp a =
    { created : Int
    , modified : Int
    , accessed : Int
    , value : a
    }

timestamp : IO a db b -> IO (Timestamp a) db (Timestamp b)
timestamp other = 
    entity Timestamp Timestamp
    |> attribute "created" int .created
    |> attribute "modified" int .modified
    |> attribute "accessed" int .accessed
    |> substruct "value" other .value 

{-
timestamp : IO b db d -> IO (Timestamp b) db (Timestamp b)
timestamp old =
    { decoder = Decode.succeed Timestamp
                |> required "created" int
                |> required "modified" int
                |> required "accessed" int
                |> required "value" old.decoder
    , toString = \name -> Maybe.andThen (old.toString name)
    , encoder = map_encoder_maybe old.encoder
    , fuzzer = Fuzz.maybe old.fuzzer
    , viewer = \db full -> Just full
    , empty = Timestamp int.empty int.empty int.empty old.empty
    }
-}

type Msg
    = All Time.Posix
    | Created Time.Posix
    | Modified Time.Posix
    | Accessed Time.Posix