module Type.Entities.User exposing (..)

import Dict exposing (Dict)
import Json.Decode
import Json.Decode.Pipeline
import Json.Encode
import Type.Entity as Entity exposing (Entity, int, maybe, string)


type alias UserFlag =
    { id : Int
    , name : Maybe String
    , age : Maybe Int
    }


type alias User =
    { id : Int
    , name : Maybe String
    , age : Maybe Int
    , answers : Answer
    }


type UserInstance
    = UserInstance User


type alias AnswerFlag =
    { id : String
    , user : Int
    , text : String
    }


type alias Answer = 
    { id : String
    , user : Int
    , text : String
    }



-- List
-- Foreign
answer : Entity AnswerFlag Answer mediator
answer =
    Entity.new AnswerFlag Answer
    |> Entity.attribute "id" .id string
    |> Entity.attribute "user_id" .user int
    |> Entity.attribute "text" .id string
    |> Entity.finish

user : Entity UserFlag
user =
    Entity.new UserFlag
    |> Entity.attribute "id" .id int
    |> Entity.attribute "name" .name (maybe string)
    |> Entity.attribute "age" .age (maybe int)
    |> Entity.relationshipNonConsuming "answers" .id (\key mediator -> Dict.get key (.answers mediator)) answer
    -- |> Entity.relationship "answers" transparent  .id .answers .user answer
    |> Entity.finish


-- decoder : Json.Decode.Decoder User
-- decoder =
--     Json.Decode.succeed User
--         |> Json.Decode.Pipeline.required "id" Json.Decode.string
--         |> Json.Decode.Pipeline.optional "name" (Json.Decode.map Just Json.Decode.string) Nothing
--         |> Json.Decode.Pipeline.optional "age" (Json.Decode.map Just Json.Decode.int) Nothing


-- encoder : User -> Json.Encode.Value
-- encoder user =
--     case user.name of
--         Just name ->
--             Json.Encode.object
--                 [ ( "id", Json.Encode.string user.id )
--                 , ( "name", Json.Encode.string name )
--                 ]

--         Nothing ->
--             Json.Encode.object
--                 [ ( "id", Json.Encode.string user.id )
--                 ]
