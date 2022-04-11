module Type.IO.ToString exposing (Error(..), ToString, array, attribute, bool, dict, entity, float, int, l2s, list, map_array_toString, map_dict_toString, map_list_toString, maybe, parseHeadTail, reference, references, result, string, substruct)

import Array exposing (Array)
import Dict exposing (Dict)
import List.Extra
import Result.Extra
import Type.IO.Internal as Id exposing (Id, box, unbox)


type Error
    = NotFound
    | IndexOutOfBounds Int
    | NotAnInt String
    | NoSuchKey String
    | NoSuchValue
    | NoSuchSubstruct String


type alias ToString a =
    String -> a -> Result Error String


int : ToString Int
int =
    \_ i -> Ok (String.fromInt i)


string : ToString String
string =
    \_ s -> Ok s


float : ToString Float
float =
    \_ f -> Ok (String.fromFloat f)


bool : ToString Bool
bool _ b =
    Ok
        (if b then
            "TRUE"

         else
            "FALSE"
        )


maybe : ToString a -> ToString (Maybe a)
maybe old name value =
    case value of
        Just v ->
            old name v

        Nothing ->
            Ok "Nothing"



-- Maybe.andThen (old name)


l2s : List String -> String
l2s l =
    "[ " ++ String.join ", " l ++ " ]"


list : ToString a -> ToString (List a)
list old name l =
    let
        ( head, rest ) =
            parseHeadTail name
    in
    if name == "*" then
        List.map (old "*") l
            |> Result.Extra.combine
            |> Result.map l2s

    else
        case String.toInt head of
            Just index ->
                case List.Extra.getAt index l of
                    Just value ->
                        old rest value

                    Nothing ->
                        Err (IndexOutOfBounds index)

            Nothing ->
                Err (NotAnInt head)



-- |> Maybe.andThen (\x -> List.Extra.getAt x l)
-- |> Maybe.andThen (old rest)
--map_list_toString old


dict : ToString comparable -> ToString a -> ToString (Dict comparable a)
dict keys values name dictionary =
    let
        ( head, rest ) =
            parseHeadTail name

        lkey : List comparable
        lkey =
            Dict.keys dictionary
                |> List.filter
                    (\x ->
                        keys rest x
                            |> Result.map (\y -> y == head)
                            |> Result.withDefault False
                    )
    in
    case List.head lkey of
        Just key ->
            case Dict.get key dictionary of
                Just value ->
                    values rest value

                Nothing ->
                    Err NoSuchValue

        Nothing ->
            Err (NoSuchKey head)



-- |> Maybe.andThen (\x -> Dict.get x dictionary)
-- |> Maybe.andThen (values rest)


result : ToString err -> ToString val -> ToString (Result err val)
result err val =
    \s x ->
        case x of
            Ok o ->
                val s o

            Err e ->
                err s e


entity : a -> ToString b
entity _ name _ =
    Err (NoSuchSubstruct name)


attribute : String -> ToString a -> (c -> a) -> ToString c -> ToString c
attribute name def getter parent =
    \acc value ->
        let
            ( head, tail ) =
                parseHeadTail acc
        in
        if name == head then
            def tail (getter value)

        else if acc == "*" then
            def "*" (getter value)
                |> Result.map (\x -> name ++ ":" ++ x ++ "\n")
                |> Result.map
                    (\x ->
                        case parent acc value of
                            Ok s ->
                                x ++ s

                            Err _ ->
                                x
                    )

        else
            parent acc value


reference : String -> (c -> Id a comparable) -> ToString comparable -> ToString c -> ToString c
reference name getter def parent =
    \acc value ->
        let
            ( head, _ ) =
                parseHeadTail acc
        in
        if name == head then
            def acc (Id.unbox (getter value))

        else if acc == "*" then
            def "*" (Id.unbox (getter value))
                |> Result.map (\x -> name ++ ":" ++ x ++ "\n")
                |> Result.map
                    (\x ->
                        case parent acc value of
                            Ok s ->
                                x ++ s

                            Err _ ->
                                x
                    )

        else
            parent acc value


references : String -> (c -> List comparable) -> ToString comparable -> ToString c -> ToString c
references name getter def parent =
    \acc value ->
        let
            ( head, tail ) =
                parseHeadTail acc
        in
        if name == head || name == "*" then
            list def tail <| getter value

        else
            parent acc value


substruct : String -> ToString a -> (c -> a) -> ToString c -> ToString c
substruct name struct getter old =
    \acc value ->
        let
            ( head, tail ) =
                parseHeadTail acc
        in
        if name == head || name == "*" then
            struct tail (getter value)

        else
            old acc value


array : ToString a -> ToString (Array a)
array old name arr =
    let
        ( head, rest ) =
            parseHeadTail name
    in
    case String.toInt head of
        Just index ->
            case Array.get index arr of
                Just value ->
                    old rest value

                Nothing ->
                    Err (IndexOutOfBounds index)

        Nothing ->
            Err (NotAnInt head)



-- map_array_toString old


map_list_toString : (String -> kind -> Maybe String) -> String -> List kind -> Maybe String
map_list_toString old s l =
    let
        ( head, rest ) =
            parseHeadTail s
    in
    String.toInt head
        |> Maybe.andThen (\x -> List.Extra.getAt x l)
        |> Maybe.andThen (old rest)


parseHeadTail : String -> ( String, String )
parseHeadTail accessor =
    if accessor == "*" then
        ( "*", "*" )

    else
        let
            rest : String
            rest =
                String.split "." accessor
                    |> List.tail
                    |> Maybe.map (String.join ".")
                    |> Maybe.withDefault ""

            index : String
            index =
                String.split "." accessor
                    |> List.head
                    |> Maybe.withDefault ""
        in
        ( index, rest )


map_dict_toString : (String -> Maybe comparable) -> (String -> value -> Maybe String) -> String -> Dict comparable value -> Maybe String
map_dict_toString key_parser value s d =
    let
        ( head, rest ) =
            parseHeadTail s
    in
    key_parser head
        |> Maybe.andThen (\x -> Dict.get x d)
        |> Maybe.andThen (value rest)



-- map_result_toString : (String -> kind -> Maybe String) -> String -> Result error kind -> Maybe String
-- map_result_toString old s r =
--     Debug.todo ""


map_array_toString : (String -> kind -> Maybe String) -> String -> Array kind -> Maybe String
map_array_toString old s a =
    let
        ( head, rest ) =
            parseHeadTail s
    in
    String.toInt head
        |> Maybe.andThen (\x -> Array.get x a)
        |> Maybe.andThen (old rest)
