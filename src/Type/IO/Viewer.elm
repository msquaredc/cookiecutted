module Type.IO.Viewer exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Maybe.Extra


type alias Viewer db full view =
    db -> full -> Maybe view


type alias Basic db kind =
    db -> kind -> Maybe kind


basic : Basic db kind
basic _ full =
    Just full


int : Basic db Int
int =
    basic


string : Basic db String
string =
    basic


float : Basic db Float
float =
    basic


bool : Basic db Bool
bool =
    basic


maybe : Viewer db a b -> Viewer db (Maybe a) (Maybe b)
maybe old db full =
    Maybe.map (old db) full


list : Viewer db a b -> Viewer db (List a) (List b)
list old db full =
    Maybe.Extra.combine (List.map (old db) full)


dict : Viewer db comparableA comparableB -> Viewer db a b -> Viewer db (Dict comparableA a) (Dict comparableB b)
dict keys values db full =
    Dict.toList full
        |> List.map (\( k, v ) -> ( keys db k, values db v ))
        |> List.map
            (\( k, v ) ->
                case ( k, v ) of
                    ( Just k1, Just v1 ) ->
                        Just ( k1, v1 )

                    ( _, _ ) ->
                        Nothing
            )
        |> Maybe.Extra.combine
        |> Maybe.map Dict.fromList


result : Viewer db errA errB -> Viewer db valA valB -> Viewer db (Result errA valA) (Result errB valB)
result err val db full =
    case full of
        Ok o ->
            val db o
                |> Maybe.map Ok

        Err e ->
            err db e
                |> Maybe.map Err


array : Viewer db a b -> Viewer db (Array a) (Array b)
array old db full =
    Array.map (old db) full
        |> Maybe.Extra.combineArray


entity : a -> Viewer db b a
entity view _ _ =
    Just view


attribute : (c -> a) -> Viewer db c (a -> d) -> Viewer db c d
attribute getter parent db full =
    Maybe.map (\x -> x (getter full)) (parent db full)


reference :
    (c -> comparable) --keygetter
    -> (db -> e) --dbgetter
    -> (comparable -> e -> Maybe f) --Dict.get
    -> (f -> a) -- .value
    -> Viewer db c (a -> d)
    -> Viewer db c d
reference keygetter dictgetter foreigngetter post parent db full =
    foreigngetter (keygetter full) (dictgetter db)
        |> Maybe.map post
        |> (\x ->
                case ( x, parent db full ) of
                    ( Just argument, Just f ) ->
                        f argument
                            |> Just

                    ( _, _ ) ->
                        Nothing
           )


references :
    (c -> List comparable)
    -> (db -> e) --dbgetter
    -> (comparable -> e -> Maybe f) --Dict.get
    -> (f -> a) -- .value
    -> Viewer db c (List a -> d)
    -> Viewer db c d
references keygetter dictgetter foreigngetter post parent db full =
    case parent db full of
        Just f ->
            keygetter full
                |> List.map (\x -> foreigngetter x (dictgetter db))
                |> Maybe.Extra.combine
                |> Maybe.map (List.map post)
                |> Maybe.map f

        _ ->
            Nothing


substruct : (e -> a) -> Viewer db a b -> Viewer db e (b -> d) -> Viewer db e d
substruct getter struct old =
    \db full ->
        case ( old db full, struct db (getter full) ) of
            ( Just f, Just v ) ->
                Just (f v)

            ( _, _ ) ->
                Nothing
