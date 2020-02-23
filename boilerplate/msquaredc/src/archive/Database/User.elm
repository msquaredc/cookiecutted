module Type.Database.User exposing (..)

import Json.Encode
import Json.Decode 
import Json.Decode.Pipeline
--import Type.Database.Description as Description

type alias User = 
    {
        id: Int,
        name : Maybe String
    }

scheme : Table
scheme =
    table
    |> int "id"
    |> optional string "name"

insert : User -> Database {a | users : Table Users} -> Database {a | users : Table Users}
insert user db = 
    let
        row = Table.row db.users
              |> int "id" user.id
        newRow = case user.name of
                    Just name->
                        row
                        |> string "name" name
                
                    Nothing -> 
                        row
              
    in
        db.users
        |> Database.insert newRow


decoder : Json.Decode.Decoder User
decoder =
    Json.Decode.succeed User
        |> Json.Decode.Pipeline.required "id" Json.Decode.int
        |> Json.Decode.Pipeline.optional "name" (Json.Decode.map Just Json.Decode.string) Nothing

encoder : User -> Json.Encode.Value
encoder user =
    case user.name of
        Just name ->
            Json.Encode.object 
                [
                    ("id", Json.Encode.int user.id),
                    ("name", Json.Encode.string name)
                ]
        Nothing ->
            Json.Encode.object 
                [
                    ("id", Json.Encode.int user.id)
                ]
    