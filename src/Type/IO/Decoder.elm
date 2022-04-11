module Type.IO.Decoder exposing (Decoder, array, attribute, bool, decodeDictFromTuples, dict, entity, float, int, list, maybe, reference, references, result, string, substruct)

import Array exposing (Array)
import Dict exposing (Dict)
import Json.Decode exposing (Decoder, map, null, nullable, succeed)
import Json.Decode.Extra
import Json.Decode.Pipeline exposing (required)


type alias Decoder a =
    Json.Decode.Decoder a


int : Decoder Int
int =
    Json.Decode.int


string : Decoder String
string =
    Json.Decode.string


float : Decoder Float
float =
    Json.Decode.float


bool : Decoder Bool
bool =
    Json.Decode.bool


maybe : Decoder a -> Decoder (Maybe a)
maybe =
    Json.Decode.nullable


list : Decoder a -> Decoder (List a)
list =
    Json.Decode.list


dict : (String -> Decoder comparable) -> Decoder a -> Decoder (Dict comparable a)
dict key value =
    Json.Decode.keyValuePairs value
        |> Json.Decode.andThen (decodeDictFromTuples key)



-- let
--     decodeToMaybe : Decoder comparable -> ( String, b ) -> Maybe ( comparable, b )
--     decodeToMaybe d ( s, obj ) =
--         Json.Decode.decodeString d s
--             |> Result.toMaybe
--             |> Maybe.map (\x -> ( x, obj ))
-- in
--     Json.Decode.keyValuePairs value
--         |> Json.Decode.map (List.filterMap (decodeToMaybe key))
--         |> Json.Decode.map Dict.fromList


decodeDictFromTuples :
    (String -> Decoder comparable)
    -> List ( String, v )
    -> Decoder (Dict comparable v)
decodeDictFromTuples keyDecoder tuples =
    case tuples of
        [] ->
            succeed Dict.empty

        ( strKey, value ) :: rest ->
            case Json.Decode.decodeString (keyDecoder strKey) strKey of
                Ok key ->
                    decodeDictFromTuples keyDecoder rest
                        |> Json.Decode.andThen (Dict.insert key value >> succeed)

                Err error ->
                    case Json.Decode.decodeString (keyDecoder strKey) "{}" of
                        Ok key ->
                            decodeDictFromTuples keyDecoder rest
                                |> Json.Decode.andThen (Dict.insert key value >> succeed)

                        Err _ ->
                            Json.Decode.fail (Json.Decode.errorToString error)


result : Decoder err -> Decoder a -> Decoder (Result err a)
result _ val =
    Json.Decode.map (\x -> Ok x) val


array : Decoder a -> Decoder (Array a)
array =
    Json.Decode.array


entity : a -> Decoder a
entity =
    Json.Decode.succeed


attribute : String -> Decoder a -> Decoder (a -> b) -> Decoder b
attribute name child parent =
    parent |> required name child


reference : String -> Decoder a -> Decoder (a -> b) -> Decoder b
reference =
    attribute


references : String -> Decoder a -> Decoder (List a -> b) -> Decoder b
references name child parent =
    parent |> required name (Json.Decode.list child)


substruct : String -> Decoder a -> Decoder (a -> b) -> Decoder b
substruct =
    attribute
