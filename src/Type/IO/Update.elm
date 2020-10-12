module Type.IO.Update exposing (..)

import List.Extra
import Dict exposing (Dict)
import Array exposing (Array)
import Array.Extra


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

type Msg
    = IntMsg (Int -> Int)
    | StringMsg (String -> String)
    | FloatMsg (Float -> Float)
    | BoolMsg (Bool -> Bool)
    | AttributeMsg String Msg
    | MaybeMsg Msg
    | MaybeSetMsg (Maybe Msg)
    | ListMsg Int Msg
    | DictValueMsg String Msg
    | ResultErrMsg Msg
    | ResultOkMsg Msg
    | ArrayMsg Int Msg



updateWithLong : (car -> string) -> String -> Updater string -> Msg -> car  ->  string
updateWithLong getter name def message car =
    case message of
        AttributeMsg searched msg_ ->
            if name == searched then
                def msg_ (getter car)
            else 
                getter car
        _ ->
            getter car


type alias PartialUpdater car string =
    (Msg -> car -> string)

type alias Updater a = 
    PartialUpdater a a

int : Updater Int
int msg val = 
    case msg of
        IntMsg f ->
            f val
            
        _ ->
            val

string : Updater String
string msg val = 
    case msg of
        StringMsg f ->
            f val
            
        _ ->
            val

float : Updater Float
float msg val = 
    case msg of
        FloatMsg f ->
            f val
            
        _ ->
            val
            
bool : Updater Bool
bool msg val = 
    case msg of
        BoolMsg f ->
            f val
            
        _ ->
            val

maybe : Updater a -> Updater (Maybe a)
maybe old msg val =
    case msg of
        MaybeMsg msg_ ->
            case val of
                Just v ->
                    Just (old msg_ v)
            
                Nothing ->
                    Nothing
        MaybeSetMsg msg_ ->
            case msg_ of
                Just msg__ ->
                    maybe old (MaybeMsg (msg__)) val
                Nothing ->
                    Nothing
        _ ->
            val
-- TODO: Allow to Set


list : Updater a -> Updater (List a)
list old msg val =
    case msg of
        ListMsg index msg_ ->
            List.Extra.updateAt index (old msg_) val
        _ ->
            val

dict : (comparable -> Maybe String) -> Updater comparable -> Updater a -> Updater (Dict comparable a)
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
                |> List.map (\x -> (x, Dict.get x val))
                |> List.filterMap (\(x,y) -> case y of
                    Just v->
                        (Just (x,v))
                
                    _ ->
                        Nothing
                )
                |> List.map (\(x,y) ->(x, values msg_ y))
                |> Dict.fromList
                |> (\x -> Dict.union x val)
        --TODO: Map keys
        _ ->
            val

result : Updater err -> Updater a -> Updater (Result err a)
result err ok msg val =
    case (msg, val) of
        (ResultErrMsg msg_, Err error) ->
            Err (err msg_ error)
        (ResultOkMsg msg_, Ok value ) ->
            Ok (ok msg_ value)
        _ -> val
            
array : Updater a -> Updater (Array a)
array old msg val =
    case msg of
        ArrayMsg index msg_ ->
            Array.Extra.update index (old msg_) val
    
        _ ->
            val
    


entity:  a -> PartialUpdater car a
entity toChange msg car = toChange


attribute : String -> (car -> string) -> Updater string -> PartialUpdater car (string -> b) -> PartialUpdater car b
attribute name getter def parent msg car =
    (parent msg car) (updateWithLong getter name def msg car)

reference : String -> (car -> comparable) ->  Updater comparable -> PartialUpdater car (comparable -> b) -> PartialUpdater car b
reference = attribute

references : String -> (car -> (List comparable)) -> Updater comparable -> PartialUpdater car ((List comparable) -> b) -> PartialUpdater car b
references name getter def =
    attribute name getter (list def)

substruct : String -> (car -> string) -> Updater string -> PartialUpdater car (string -> b) -> PartialUpdater car b
substruct = attribute


carUpdater2 : Updater Car
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