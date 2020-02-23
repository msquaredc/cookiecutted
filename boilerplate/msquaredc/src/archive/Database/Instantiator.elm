module Type.Database.Instantiator exposing (..)

import Type.Database.Type exposing (DatabaseType(..))
import Type.Database.Table exposing (Table, Row)
import Dict exposing (Dict)

type Instantiator a =
    Instantiator (Row a -> a)

{-
instantiateUser : Instantiator User 
instantiateUser = 
    instantiate User
    |> int "name"

Instantiator a =
    Dict Sting Types -> a
-}


custom : Instantiator a -> Instantiator (a -> b) -> Instantiator (b)
custom =
    map2 (|>) 

map2 : (a -> b -> c) -> Instantiator a -> Instantiator b -> Instantiator c
map2 f (Instantiator first) (Instantiator second) =
    Instantiator (f first second)

instantiate : a -> Instantiator a
instantiate element =
    Instantiator ({target = element})

required : String -> Instantiator a -> Instantiator (a -> b) -> Instantiator (b)
required key valInstantiator instantiator = 
    custom (field key valInstantiator) instantiator

field : String -> Instantiator a -> Instantiator a
field =

int : Instantiator Int 
int = 
    Instantiator (4)



