module Type.IO.Update exposing (Car, Msg(..), PartialUpdater, Person, Updater, array, attribute, bool, carUpdater2, dict, entity, float, int, list, maybe, person_str_updater, reference, references, result, string, substruct, updateWithLong)

import Array exposing (Array)
import Array.Extra
import Dict exposing (Dict)
import List.Extra


type alias Person =
    { name : String }


person_str_updater : (Person -> String) -> (String -> String) -> Person -> Person
person_str_updater getter f x =
    Person (f (getter x))


type alias Car =
    { brand : String
    , model : String
    , age : Int
    }


type Msg a
    = IntMsg (Int -> Int)
    | StringMsg (String -> String)
    | FloatMsg (Float -> Float)
    | BoolMsg (Bool -> Bool)
    | AttributeMsg String (Msg a)
    | MaybeMsg (Msg a)
    | MaybeSetMsg (Maybe (Msg a)) a
    | ListMsg Int (Msg a)
    | DictValueMsg String (Msg a)
    | ResultErrMsg (Msg a)
    | ResultOkMsg (Msg a)
    | ArrayMsg Int (Msg a)


updateWithLong : (car -> string) -> String -> Updater string a -> Msg a -> car -> string
updateWithLong getter name def message car =
    case message of
        AttributeMsg searched msg_ ->
            if name == searched then
                def msg_ (getter car)

            else
                getter car

        _ ->
            getter car


type alias PartialUpdater car string a =
    Msg a -> car -> string


type alias Updater a b =
    PartialUpdater a a b


int : Updater Int a
int msg val =
    case msg of
        IntMsg f ->
            f val

        _ ->
            val


string : Updater String a
string msg val =
    case msg of
        StringMsg f ->
            f val

        _ ->
            val


float : Updater Float a
float msg val =
    case msg of
        FloatMsg f ->
            f val

        _ ->
            val


bool : Updater Bool a
bool msg val =
    case msg of
        BoolMsg f ->
            f val

        _ ->
            val


maybe : Updater a a -> Updater (Maybe a) a
maybe old msg val =
    case msg of
        MaybeMsg msg_ ->
            case val of
                Just v ->
                    Just (old msg_ v)

                Nothing ->
                    Nothing

        MaybeSetMsg msg_ default ->
            case msg_ of
                Just msg__ ->
                    Just (old msg__ default)

                Nothing ->
                    Nothing

        _ ->
            val



-- TODO: Allow to Set


list : Updater a b -> Updater (List a) b
list old msg val =
    case msg of
        ListMsg index msg_ ->
            List.Extra.updateAt index (old msg_) val

        _ ->
            val


dict : (comparable -> Maybe String) -> Updater comparable b -> Updater a b -> Updater (Dict comparable a) b
dict keySerializer keys values msg val =
    case msg of
        DictValueMsg parsedkey msg_ ->
            Dict.keys val
                |> List.filter
                    (\x ->
                        keySerializer x
                            |> Maybe.map (\y -> y == parsedkey)
                            |> Maybe.withDefault False
                    )
                |> List.map (\x -> ( x, Dict.get x val ))
                |> List.filterMap
                    (\( x, y ) ->
                        case y of
                            Just v ->
                                Just ( x, v )

                            _ ->
                                Nothing
                    )
                |> List.map (\( x, y ) -> ( x, values msg_ y ))
                |> Dict.fromList
                |> (\x -> Dict.union x val)

        --TODO: Map keys
        _ ->
            val


result : Updater err b -> Updater a b -> Updater (Result err a) b
result err ok msg val =
    case ( msg, val ) of
        ( ResultErrMsg msg_, Err error ) ->
            Err (err msg_ error)

        ( ResultOkMsg msg_, Ok value ) ->
            Ok (ok msg_ value)

        _ ->
            val


array : Updater a b -> Updater (Array a) b
array old msg val =
    case msg of
        ArrayMsg index msg_ ->
            Array.Extra.update index (old msg_) val

        _ ->
            val


entity : a -> PartialUpdater car a b
entity toChange msg car =
    toChange


attribute : String -> (car -> string) -> Updater string c -> PartialUpdater car (string -> b) c -> PartialUpdater car b c
attribute name getter def parent msg car =
    parent msg car (updateWithLong getter name def msg car)


reference : String -> (car -> comparable) -> Updater comparable a -> PartialUpdater car (comparable -> b) a -> PartialUpdater car b a
reference =
    attribute


references : String -> (car -> List comparable) -> Updater comparable a -> PartialUpdater car (List comparable -> b) a -> PartialUpdater car b a
references name getter def =
    attribute name getter (list def)


substruct : String -> (car -> string) -> Updater string a -> PartialUpdater car (string -> b) a -> PartialUpdater car b a
substruct =
    attribute


carUpdater2 : Updater Car a
carUpdater2 =
    entity Car
        |> attribute "brand" .brand string
        |> reference "model" .model string
        |> attribute "age" .age int



-- first = (updateWith )
-- carUpdater : Msg -> Car -> Car
-- carUpdater msg car =
-- -- Car = (String -> String -> Int -> Car)
-- attribute : (car -> string) -> (string -> other) ->
