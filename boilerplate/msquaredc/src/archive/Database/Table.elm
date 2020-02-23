module Type.Database.Table exposing (..)

import Dict exposing (Dict)
import Type.Type exposing (Type)
import Type.Database.Scheme as Scheme exposing (Scheme)

type Row a = 
    Row (Dict String (Type a))


type Table a = 
    Table {
        scheme : Scheme a,
        rows : List (Row a)
    }

table : Table a
table = Table {
        scheme = Scheme.scheme,
        rows = []
    }

fromScheme : Scheme a -> Table a
fromScheme scheme = 
    Table {scheme = scheme,
            rows = []}

addColumn : String -> Type a -> Table a -> Table a
addColumn name typus (Table t) =
    let 
        eval typus2 = 
            
    Scheme.add t.scheme
    

