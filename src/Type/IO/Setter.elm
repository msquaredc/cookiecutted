module Type.IO.Setter exposing (..)

import List.Extra
import Result.Extra
import Dict exposing (Dict)
import Array exposing (Array)
import Array.Extra
import Type.IO.Internal as Id exposing (Id)

type alias Person =
    { name : String }


person_str_updater : (Person -> String) -> (String -> String) -> Person -> Person
person_str_updater getter f x =
    Person (f (getter x))


type alias Car =
    { brand : Maybe String
    , model : Id Person String
    , age : Int
    }

type Msg
    = IntMsg (Int)
    | IntUpdate (Int -> Int)
    | StringMsg (String)
    | FloatMsg (Float)
    | BoolUpdateMsg (Bool -> Bool)
    | BoolMsg (Bool)
    | AttributeMsg String Msg
    | MaybeUpdateMsg (Maybe Msg)
    | MaybeSetMsg (Maybe Msg)
    | ListUpdateMsg Int Msg
    | ListAppendMsg Msg
    | ListMixedUpdate Int Msg
    | DictKeyMsg String Msg
    | DictAddMsg Msg Msg
    | ResultErrMsg Msg
    | ResultOkMsg Msg
    | ArrayUpdateIndexMsg Int Msg
    | ArrayAppend Msg
    | ErrorMsg
    | Custom String (Maybe Msg)

type Error =
    Mismatch Msg Msg
    | IndexOutOfBounds
    | KeyError
    | InvalidValue
    | KeyAlreadyPresent
    | CustomError String

errToString : Error -> String
errToString err =
    case err of
        Mismatch got expect ->
            "The change message didn't match up the structure: Got \"" ++ toString got ++ "\" but expected " ++ toString expect ++ "!"
    
        IndexOutOfBounds ->
            "The index was out of the bounds!"
        
        KeyError -> 
            "Didn't find this key in my dict!"
        
        InvalidValue ->
            "Cannot update this type with this value."

        KeyAlreadyPresent ->
            "Cannot create new value here. There is already a value on this key"

        CustomError s -> 
            s
        
            
    

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
        IntUpdate f ->
            Ok <| f val
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
        
        BoolMsg b ->
            Ok (b)
        _ ->
            Err <| Mismatch msg (BoolMsg val)


maybe : a -> Updater a -> Updater (Maybe a)
maybe empty old msg val =
    case msg of
        MaybeUpdateMsg (Just _)->
            case val of
                Just v ->
                    case (old msg v) of
                        Ok res ->
                            Ok (Just res)
                        Err e ->
                            Err e
                Nothing ->
                    Ok Nothing
        MaybeUpdateMsg (Nothing) ->
            Ok Nothing
        MaybeSetMsg (Just msg_) ->
            Result.map Just <| old msg_ empty
        MaybeSetMsg (Nothing) ->
            Ok Nothing
        _ ->
            Err <| Mismatch msg (MaybeUpdateMsg Nothing)
-- TODO: Allow to Set


list : a -> Updater a -> Updater (List a)
list empty old msg val =
    case msg of
        ListUpdateMsg index msg_ ->
            List.Extra.getAt index val
            |> Result.fromMaybe (IndexOutOfBounds)
            |> Result.andThen (old msg_)
            |> Result.map (\x -> List.Extra.setAt index x val)
            -- |> Maybe.map (old msg_)
            -- |> Maybe.map (\x -> List.Extra.setAt index x val)
            -- |> Result.fromMaybe (Err IndexOutOfBounds)
            -- |> Maybe.withDefault val

        ListAppendMsg msg_ ->
            case old msg_ empty of
                Ok value ->
                    Ok <| List.append val [value]
                Err err ->
                    Err err
        ListMixedUpdate index msg_ ->
            if index >= List.length val then
                list empty old msg (val ++ [empty])
            else
                list empty old (ListUpdateMsg index msg_) val
                
        _ ->
            Err <| Mismatch msg (ListUpdateMsg 0 ErrorMsg)

dict : comparable -> a -> (comparable -> Maybe String) -> Updater comparable -> Updater a -> Updater (Dict comparable a)
dict emptyKey emptyValue keySerializer keys values msg val =
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
        DictAddMsg keymsg_ valuemsg_ ->
            case (keys keymsg_ emptyKey, values valuemsg_ emptyValue) of
                (Err err,_) ->
                    Err err
                (_, Err err) ->
                    Err err
                (Ok key, Ok value) ->
                    if Dict.member key val then
                        Err KeyAlreadyPresent
                    else
                        Ok <| Dict.insert key value val
        
        --TODO: Map keys
        _ ->
            Err <| Mismatch msg (DictKeyMsg "" ErrorMsg)

result : err -> a -> Updater err -> Updater a -> Updater (Result err a)
result emptyErr emptyOk err ok msg val =
    case (msg, val) of
        (ResultErrMsg msg_, Err error) ->
            (err msg_ error)
            |> Result.map Err
        (ResultOkMsg msg_, Ok value ) ->
            (ok msg_ value)
            |> Result.map Ok
        _ ->  
            Err <| Mismatch msg (ResultErrMsg ErrorMsg)
     
array : a -> Updater a -> Updater (Array a)
array empty old msg val =
    case msg of
        ArrayUpdateIndexMsg index msg_ ->
            Array.get index val
            |> Result.fromMaybe IndexOutOfBounds
            |> Result.andThen (old msg_)
            |> Result.map (\x -> Array.set index x val)
        ArrayAppend msg_ ->
            old msg_ empty
            |> Result.map (\x -> Array.push x val)
        _ ->
            Err <| Mismatch msg (ArrayUpdateIndexMsg 0 ErrorMsg)
    


entity:  a -> PartialUpdater car a
entity toChange msg car = Ok toChange


attribute : String -> (car -> string) -> Updater string -> PartialUpdater car (string -> b) -> PartialUpdater car b
attribute name getter def parent msg car =
    (parent msg car) 
    |> Result.map2 (\x y -> y x) (updateWithLong getter name def msg car)

reference : String -> (car -> Id a comparable) ->  Updater comparable -> PartialUpdater car ((Id a comparable) -> b) -> PartialUpdater car b
reference name getter def parent msg car =
    let
        iddef msg_ car_ = def msg_ (Id.unbox car_)
                               |> Result.map (Id.box)
    in
    (parent msg car) 
    |> Result.map2 (\x y -> y x) (updateWithLong getter name iddef msg car)


references : String -> (car -> (List comparable)) -> comparable -> Updater comparable -> PartialUpdater car ((List comparable) -> b) -> PartialUpdater car b
references name getter empty def =
    attribute name getter (list empty def)

substruct : String -> (car -> string) -> Updater string -> PartialUpdater car (string -> b) -> PartialUpdater car b
substruct = attribute


carUpdater2 : Updater Car
carUpdater2 =
    entity Car
    |> attribute "brand" .brand (maybe "" string)
    |> reference "model" .model string
    |> attribute "age" .age int

car1 : Car
car1 = Car Nothing (Id.box "mymodel") 12 

car2 : Result Error Car
car2 = carUpdater2 (AttributeMsg "brand" (MaybeUpdateMsg (Just (StringMsg "Hello")))) car1


-- first = (updateWith )

-- carUpdater : Msg -> Car -> Car
-- carUpdater msg car =


-- -- Car = (String -> String -> Int -> Car)

-- attribute : (car -> string) -> (string -> other) -> 

toString : Msg -> String
toString msg =
    case msg of
        IntMsg v -> 
            "(int) " ++ String.fromInt v
        IntUpdate _ ->
            "(int) updating"
        StringMsg s ->
            "(string) " ++ s
        FloatMsg f ->
            "(float) " ++ String.fromFloat f
        AttributeMsg a msg_ ->
            a ++ "->" ++ toString msg_
        MaybeUpdateMsg (Just msg_) ->
            "Just (update) ->" ++ toString msg_
        MaybeUpdateMsg Nothing ->
            "Nothing (update)"
        MaybeSetMsg (Just msg_) ->
            "Just (set) ->" ++ toString msg_
        MaybeSetMsg (Nothing) ->
            "Nothing (set)"
        Custom name (Just msg_) ->
            name ++ " (custom) ->" ++ toString msg_
        Custom name Nothing ->
            name ++ " (custom)"
        BoolMsg b ->
            "(bool) " ++ if b then
                            "TRUE"
                         else
                            "FALSE"
        _ -> "Yet undeclared!"