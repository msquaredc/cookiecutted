module Session exposing (Session, init)

import Json.Decode
import Time
import Type.Database as Db
import Type.Database.TypeMatching as Match
import Type.Flags
import Type.IO.Internal exposing (Id)



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
    , user : Maybe (Id Db.User String)

    --    , localStorage : Maybe Type.LocalStorage.LocalStorage
    , db : Db.Database
    }



-- Initializes a session given some flags


init : Type.Flags.Flags -> Session
init flags =
    let
        --        localStorage =
        --            Json.Decode.decodeValue Type.LocalStorage.decode flags.localStorage
        db =
            Json.Decode.decodeValue Db.database.decoder flags.db

        posixTime =
            Time.millisToPosix flags.timeAppStarted
    in
    case db of
        Ok storage ->
            let
                user =
                    Match.keys Db.UserType storage
                        |> (\x ->
                                case List.length x of
                                    1 ->
                                        List.head x

                                    _ ->
                                        Nothing
                           )
            in
            Session posixTime flags.windowSize Nothing storage

        Err _ ->
            Session posixTime flags.windowSize Nothing Db.database.empty



-- Getters/Setters can go here
