module Type.IO.Setter exposing (..)

import List.Extra
import Result.Extra
import Dict exposing (Dict)
import Array exposing (Array)
import Array.Extra


type alias Person =
    { name : String }


person_str_updater : (Person -> String) -> (String -> String) -> Person -> Person
person_str_updater getter f x =
    Person (f (getter x))


type alias Car =
    { brand : Maybe String
    , model : String
    , age : Int
    }

type Msg
    = IntMsg (Int)
    | StringMsg (String)
    | FloatMsg (Float)
    | BoolUpdateMsg (Bool -> Bool)
    | BoolMsg (Bool)
    | AttributeMsg String Msg
    | MaybeMsg (Maybe Msg)
    | ListMsg Int Msg
    | DictKeyMsg String Msg
    | ResultErrMsg Msg
    | ResultOkMsg Msg
    | ArrayMsg Int Msg
    | ErrorMsg

type Error =
    Mismatch Msg Msg
    | IndexOutOfBounds
    | KeyError

errToString : Error -> String
errToString err =
    case err of
        Mismatch got expect ->
            "The change message didn't match up the structure."
    
        IndexOutOfBounds ->
            "The index was out of the bounds!"
        
        KeyError -> 
            "Didn't find this key in my dict!"
        
            
    

updateWithLong : (car -> string) -> String -> Updater string -> Msg -> car  ->  Result Error string
updateWithLong getter name def message car =
    case message of
        AttributeMsg searched msg_ ->
            if name == searched then
                def msg_ (getter car)
            else 
                Ok (getter car)
        _ ->
            Err <| Mismatch message (AttributeMsg name ErrorMsg)


type alias PartialUpdater car string =
    (Msg -> car -> Result Error string)

type alias Updater a = 
    PartialUpdater a a

int : Updater Int
int msg val = 
    case msg of
        IntMsg f ->
            Ok f
            
        _ ->
            Err <| Mismatch msg (IntMsg val)

string : Updater String
string msg val = 
    case msg of
        StringMsg f ->
            Ok f
            
        _ ->
            Err <| Mismatch msg (StringMsg val)

float : Updater Float
float msg val = 
    case msg of
        FloatMsg f ->
            Ok f
            
        _ ->
            Err <| Mismatch msg (FloatMsg val)
            
bool : Updater Bool
bool msg val = 
    case msg of
        BoolUpdateMsg f ->
            Ok (f val)
            
        _ ->
            Err <| Mismatch msg (BoolMsg val)

maybe : Updater a -> Updater (Maybe a)
maybe old msg val =
    case msg of
        MaybeMsg (Just _)->
            case val of
                Just v ->
                    case (old msg v) of
                        Ok res ->
                            Ok (Just res)
                        Err e ->
                            Err e
                Nothing ->
                    Ok Nothing
        MaybeMsg (Nothing) ->
            Ok Nothing
        _ ->
            Err <| Mismatch msg (MaybeMsg Nothing)
-- TODO: Allow to Set


list : Updater a -> Updater (List a)
list old msg val =
    case msg of
        ListMsg index msg_ ->
            List.Extra.getAt index val
            |> Result.fromMaybe (IndexOutOfBounds)
            |> Result.andThen (old msg_)
            |> Result.map (\x -> List.Extra.setAt index x val)
            -- |> Maybe.map (old msg_)
            -- |> Maybe.map (\x -> List.Extra.setAt index x val)
            -- |> Result.fromMaybe (Err IndexOutOfBounds)
            -- |> Maybe.withDefault val
        _ ->
            Err <| Mismatch msg (ListMsg 0 ErrorMsg)

dict : (comparable -> Maybe String) -> Updater comparable -> Updater a -> Updater (Dict comparable a)
dict keySerializer keys values msg val =
    case msg of
        DictKeyMsg parsedkey msg_ ->
            Dict.keys val
                |> List.filter
                    (\x ->
                            keySerializer x
                            |> Maybe.map (\y -> y == parsedkey)
                            |> Maybe.withDefault False
                    )
                |> List.map (\x -> (x, Result.fromMaybe KeyError (Dict.get x val)))
                |> List.map (\(x,y) -> (x, Result.andThen (values msg_) y))
                |> List.map (\(x,y) ->
                    case y of
                        Ok value ->
                           Ok (x,value) 
                    
                        Err err ->
                            Err err
                            
                    )
                |> Result.Extra.combine
                -- |> List.filterMap (\(x,y) -> case y of
                --     Ok v->
                --         (Just (x,v))
                
                --     _ ->
                --         Nothing
                -- )
                
                |> Result.map Dict.fromList
                |> Result.map (\x -> Dict.union x val)
        
        --TODO: Map keys
        _ ->
            Err <| Mismatch msg (DictKeyMsg "" ErrorMsg)

result : Updater err -> Updater a -> Updater (Result err a)
result err ok msg val =
    case (msg, val) of
        (ResultErrMsg msg_, Err error) ->
            (err msg_ error)
            |> Result.map Err
        (ResultOkMsg msg_, Ok value ) ->
            (ok msg_ value)
            |> Result.map Ok
        _ ->  
            Err <| Mismatch msg (ResultErrMsg ErrorMsg)
            
array : Updater a -> Updater (Array a)
array old msg val =
    case msg of
        ArrayMsg index msg_ ->
            Array.get index val
            |> Result.fromMaybe IndexOutOfBounds
            |> Result.andThen (old msg_)
            |> Result.map (\x -> Array.set index x val)
    
        _ ->
            Err <| Mismatch msg (ArrayMsg 0 ErrorMsg)
    


entity:  a -> PartialUpdater car a
entity toChange msg car = Ok toChange


attribute : String -> (car -> string) -> Updater string -> PartialUpdater car (string -> b) -> PartialUpdater car b
attribute name getter def parent msg car =
    (parent msg car) 
    |> Result.map2 (\x y -> y x) (updateWithLong getter name def msg car)

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
    |> attribute "brand" .brand (maybe string)
    |> reference "model" .model string
    |> attribute "age" .age int

car1 : Car
car1 = Car Nothing "mymodel" 12 

car2 : Result Error Car
car2 = carUpdater2 (AttributeMsg "brand" (MaybeMsg (Just (StringMsg "Hello")))) car1


-- first = (updateWith )

-- carUpdater : Msg -> Car -> Car
-- carUpdater msg car =


-- -- Car = (String -> String -> Int -> Car)

-- attribute : (car -> string) -> (string -> other) -> 