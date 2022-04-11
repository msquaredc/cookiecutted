module Type.IOTest exposing (Car, CarView, Chef, ChefView, Db, DbView, Person, PersonView, Timestamp, car, chef, db1, flip, person, timestamp)

import Dict exposing (Dict)
import Type.IO exposing (..)


type alias Car =
    { owner : String
    , name : String
    }


type alias CarView =
    { owner : Person
    , name : String
    }


flip : (a -> b -> c) -> b -> a -> c
flip f second first =
    f first second



{- timestapedlense : Db -> String -> Maybe (Person)
   timestapedlense = \db arg -> Dict.get arg (.people db)
   --                   |> Maybe.map .value

   dictlense : Db -> String -> Maybe ( Person)
   dictlense = \db arg -> Dict.get arg (.people db)
-}
{- genericlense : (Db -> Dict String a) -> (Db -> String -> Maybe a)
   genericlense mapper = \db arg -> Dict.get arg (mapper db)

   evermoregenericlense : (Db -> b) -> (comparable -> b -> Maybe a) -> (Db -> comparable -> Maybe a)
   evermoregenericlense fromDb toValue = \db arg -> toValue arg (fromDb db)

   mostgenericlense : (Db -> b) -> (b -> comparable -> Maybe a) -> (a -> c) -> (Db -> comparable -> Maybe c)
   mostgenericlense fromDb toFlag toValue = \db arg -> toFlag (fromDb db) arg
                                                       |> Maybe.map toValue
-}
{- dict2lense : Db -> String -> Maybe ( Person)
   dict2lense = evermoregenericlense .people Dict.get

   timpestaped2lense : Db -> String -> Maybe Person
   timpestaped2lense = mostgenericlense .people (flip Dict.get) identity
-}
{- l : (Db -> b) -> (b -> comparable -> Maybe a) -> (a -> c) -> (Db -> comparable -> Maybe c)
   l = mostgenericlense
-}


car : IO Car Db CarView
car =
    entity Car CarView
        |> reference "owner" string .owner .people Dict.get .value
        |> attribute "name" string .name


type alias Person =
    { cars : List String
    }


type alias PersonView =
    { cars : List Car
    }


person : IO Person Db PersonView
person =
    entity Person PersonView
        |> references "cars" string .cars .cars Dict.get .value


type alias Chef =
    { person : Person
    , name : String
    , employees : List String
    }


type alias ChefView =
    { person : PersonView
    , name : String
    , employees : List Person
    }


chef : IO Chef Db ChefView
chef =
    entity Chef ChefView
        |> substruct "person" person .person
        |> attribute "name" string .name
        |> references "employees" string .employees .people Dict.get .value


type alias Db =
    { people : Dict String (Timestamp Person)
    , cars : Dict String (Timestamp Car)
    }


type alias DbView =
    { people : Dict String PersonView
    , cars : Dict String CarView
    }


db1 : IO Db Db DbView
db1 =
    entity Db DbView
        |> substruct "people" (dict string (timestamp person)) .people
        |> substruct "cars" (dict string (timestamp car)) .cars


type alias Timestamp a =
    { created : Int
    , modified : Int
    , accessed : Int
    , value : a
    }


timestamp : IO a db b -> IO (Timestamp a) db b
timestamp other =
    let
        t =
            entity Timestamp Timestamp
                |> attribute "created" int .created
                |> attribute "modified" int .modified
                |> attribute "accessed" int .accessed
                |> substruct "value" other .value
    in
    { decoder = t.decoder
    , encoder = t.encoder
    , fuzzer = t.fuzzer
    , toString = t.toString
    , empty = t.empty
    , viewer = \db full -> Maybe.map (\x -> x.value) (t.viewer db full)
    }
