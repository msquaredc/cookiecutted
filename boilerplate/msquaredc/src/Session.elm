module Session exposing (Session, init)

import Json.Decode
import Time
import Type.Database
import Type.Flags
--import Type.LocalStorage



{-
   The session is used for any data that needs to be shared globally across all pages. All pages have the session in their model.
   You can use this to store info like credentials.
   Currently, I am storing localStorage in session, however it may be better to decode what you need from localStorage and store only that value.
-}


type alias Session =
    { timeAppStarted : Time.Posix
    , windowSize :
        { width : Int
        , height : Int
        }

    --    , localStorage : Maybe Type.LocalStorage.LocalStorage
    , db : Type.Database.Database
    }



-- Initializes a session given some flags


init : Type.Flags.Flags -> Session
init flags =
    let
        --        localStorage =
        --            Json.Decode.decodeValue Type.LocalStorage.decode flags.localStorage
        db =
            Json.Decode.decodeValue Type.Database.database.decoder flags.db

        posixTime =
            Time.millisToPosix flags.timeAppStarted
    in
    case db of
        Ok storage ->
            Session posixTime flags.windowSize storage

        Err _ ->
            Session posixTime flags.windowSize Type.Database.database.empty



-- Getters/Setters can go here
