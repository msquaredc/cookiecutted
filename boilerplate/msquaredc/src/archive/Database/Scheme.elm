module Type.Database.Scheme exposing (..)

import Dict exposing (Dict)
import Type.Type exposing (Type(..))

type Scheme a =
    Scheme (Dict String (List(Type a -> Bool)))

scheme : Scheme a
scheme =
    Scheme (Dict.empty)

validateOne : String -> (Type a) -> Scheme a -> Bool
validateOne name value (Scheme s) =
    Dict.get name s
    |> Maybe.map (List.map (\x -> x value))
    |> Maybe.map (List.foldl (&&) True)
    |> Maybe.withDefault False

validate : Scheme a -> Dict String (Type a) -> Bool
validate (Scheme s) db = 
    Dict.toList db
    |> List.map (\(x,y) -> validateOne x y (Scheme s))
    |> List.foldl (&&) True
    |> (&&) ((==) (Dict.keys s) (Dict.keys db)) 

add : String -> (Type a -> Bool) -> Scheme a -> Scheme a
add name eval (Scheme s) =
    let 
        newentry = case Dict.get name s of
                    Just tests ->
                        eval :: tests
                    Nothing ->
                        eval :: []
    in 
        Scheme (Dict.insert name newentry s)

int : String -> Scheme a -> Scheme a
int name s =
    let 
        eval t = 
            case t of 
                IntType _ ->
                    True
                _ ->
                    False
    in
        s
        |> add name eval

