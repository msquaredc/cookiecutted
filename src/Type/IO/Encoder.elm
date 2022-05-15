module Type.IO.Encoder exposing (Encoder(..), array, attribute, bool, collapseEncoder, dict, entity, float, int, list, maybe, reference, references, result, string, substruct)

import Array exposing (Array)
import Dict exposing (Dict)
import Json.Encode
import Type.IO.Internal as Id exposing (Id)


type Encoder a
    = SingleEncoder (a -> Json.Encode.Value)
    | ListEncoder (List (a -> ( String, Json.Encode.Value )))


int : Encoder Int
int =
    SingleEncoder Json.Encode.int


string : Encoder String
string =
    SingleEncoder Json.Encode.string


float : Encoder Float
float =
    SingleEncoder Json.Encode.float


bool : Encoder Bool
bool =
    SingleEncoder Json.Encode.bool


maybe : Encoder a -> Encoder (Maybe a)
maybe old =
    case old of
        ListEncoder e ->
            SingleEncoder
                (\x ->
                    case x of
                        Just v ->
                            Json.Encode.object (List.map (\y -> y v) e)

                        Nothing ->
                            Json.Encode.null
                )

        SingleEncoder e ->
            SingleEncoder
                (\x ->
                    Maybe.map e x
                        |> Maybe.withDefault Json.Encode.null
                )


list : Encoder a -> Encoder (List a)
list old =
    SingleEncoder (Json.Encode.list (collapseEncoder old))


dict : (comparable -> Maybe String) -> Encoder a -> Encoder (Dict comparable a)
dict keys values =
    SingleEncoder
        (\x ->
            Dict.toList x
                |> List.map (\( a, b ) -> ( keys a, collapseEncoder values b ))
                |> List.filterMap getMaybeOut
                |> Json.Encode.object
        )


result : Encoder err -> Encoder val -> Encoder (Result err val)
result err val =
    SingleEncoder
        (\x ->
            case x of
                Ok v ->
                    case val of
                        ListEncoder e ->
                            Json.Encode.object (List.map (\y -> y v) e)

                        SingleEncoder e ->
                            e v

                Err v ->
                    case err of
                        ListEncoder e ->
                            Json.Encode.object (List.map (\y -> y v) e)

                        SingleEncoder e ->
                            e v
        )


array : Encoder a -> Encoder (Array a)
array old =
    old
        |> collapseEncoder
        |> Json.Encode.array
        |> SingleEncoder


entity : Encoder a
entity =
    ListEncoder []


attribute : String -> Encoder a -> (c -> a) -> Encoder c -> Encoder c
attribute name def getter parent =
    case ( parent, def ) of
        ( ListEncoder pe, SingleEncoder de ) ->
            ListEncoder <|
                [ \x -> ( name, de (getter x) ) ]
                    ++ pe

        ( ListEncoder pe, ListEncoder de ) ->
            ListEncoder <|
                [ \x -> ( name, listToSingle de (getter x) ) ]
                    ++ pe

        ( SingleEncoder pe, _ ) ->
            SingleEncoder pe


reference : String -> (b -> Id c comparable) -> Encoder comparable -> Encoder b -> Encoder b
reference name getter def parent =
    case parent of
        ListEncoder pe ->
            ListEncoder
                ((\x ->
                    case def of
                        SingleEncoder e ->
                            ( name
                            , e (Id.unbox (getter x))
                            )

                        ListEncoder e ->
                            ( name
                            , collapseEncoder (ListEncoder e) (Id.unbox (getter x))
                            )
                 )
                    :: pe
                )

        SingleEncoder pe ->
            SingleEncoder pe


references : String -> (b -> List comparable) -> Encoder comparable -> Encoder b -> Encoder b
references name getter def parent =
    case parent of
        ListEncoder pe ->
            ListEncoder
                ((\x ->
                    case def of
                        SingleEncoder e ->
                            ( name
                            , Json.Encode.list e (getter x)
                            )

                        ListEncoder e ->
                            ( name
                            , Json.Encode.list (collapseEncoder (ListEncoder e)) (getter x)
                            )
                 )
                    :: pe
                )

        SingleEncoder pe ->
            SingleEncoder pe


substruct : String -> Encoder a -> (b -> a) -> Encoder b -> Encoder b
substruct name struct getter old =
    case ( old, struct ) of
        ( ListEncoder pe, SingleEncoder de ) ->
            ListEncoder <|
                [ \x -> ( name, de (getter x) ) ]
                    ++ pe

        ( ListEncoder pe, ListEncoder de ) ->
            ListEncoder <|
                [ \x -> ( name, listToSingle de (getter x) ) ]
                    ++ pe

        ( SingleEncoder pe, _ ) ->
            SingleEncoder pe


getMaybeOut : ( Maybe a, b ) -> Maybe ( a, b )
getMaybeOut mb =
    case mb of
        ( Nothing, _ ) ->
            Nothing

        ( Just a, b ) ->
            Just ( a, b )


collapseEncoder : Encoder a -> (a -> Json.Encode.Value)
collapseEncoder enc =
    case enc of
        SingleEncoder s ->
            s

        ListEncoder l ->
            \x -> Json.Encode.object (List.map (\y -> y x) l)


listToSingle : List (a -> ( String, Json.Encode.Value )) -> (a -> Json.Encode.Value)
listToSingle l =
    \x -> Json.Encode.object (List.map (\y -> y x) l)
