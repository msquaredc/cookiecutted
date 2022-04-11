module Type.UpdateTest exposing (..)

import Dict exposing (Dict)
import Type.IO.Update exposing (..)


type alias Car =
    { owner : String
    , name : String
    }


type alias CarView =
    { owner : Person
    , name : String
    }

type alias CarBuilder =
    String -> String -> Car

flip : (a -> b -> c) -> b -> a -> c
flip f second first = f first second

{- timestapedlense : Db -> String -> Maybe (Person)
timestapedlense = \db arg -> Dict.get arg (.people db)
--                   |> Maybe.map .value

dictlense : Db -> String -> Maybe ( Person)
dictlense = \db arg -> Dict.get arg (.people db) -}

{- genericlense : (Db -> Dict String a) -> (Db -> String -> Maybe a)
genericlense mapper = \db arg -> Dict.get arg (mapper db)

evermoregenericlense : (Db -> b) -> (comparable -> b -> Maybe a) -> (Db -> comparable -> Maybe a)
evermoregenericlense fromDb toValue = \db arg -> toValue arg (fromDb db)

mostgenericlense : (Db -> b) -> (b -> comparable -> Maybe a) -> (a -> c) -> (Db -> comparable -> Maybe c)
mostgenericlense fromDb toFlag toValue = \db arg -> toFlag (fromDb db) arg
                                                    |> Maybe.map toValue -}

{- dict2lense : Db -> String -> Maybe ( Person)
dict2lense = evermoregenericlense .people Dict.get

timpestaped2lense : Db -> String -> Maybe Person
timpestaped2lense = mostgenericlense .people (flip Dict.get) identity -}

{- l : (Db -> b) -> (b -> comparable -> Maybe a) -> (a -> c) -> (Db -> comparable -> Maybe c)
l = mostgenericlense -}

car : PartialUpdater Car (String -> String -> Car) Car
car =
    entity Car
        |> attribute "owner" string .owner 
        |> attribute "name" string .name


type alias Foo =
    { foo : Person
    }
-- is: ((Person -> Foo) -> Person -> Foo) :-> e = Person, c = Foo
-- 
-- expects : ((List String -> Person) -> c): -> e = PersonBuilder
-- (((List String -> Person) -> Foo) -> (List String -> Person) -> Foo) ==> expand person

type alias FooBuilder =
    PersonBuilder -> Foo



foo : PartialUpdater Foo  (FooBuilder) Foo
foo = 
    entity Foo
        |> substruct "foo" person .foo



type alias Person =
    { cars : List String
    }


type alias PersonView =
    { cars : List Car
    }

type alias PersonBuilder = 
    List String -> Person

person : PartialUpdater Person (PersonBuilder) Person
person =
    entity Person
        |> references "cars" string .cars 
        


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

type alias ChefBuilder = 
    PersonBuilder -> String -> List String -> Chef


chef : PartialUpdater Chef ChefBuilder Chef
chef =
    entity Chef
        |> substruct "person" person .person
        |> attribute "name" string .name
        |> references "employees" string .employees .people Dict.get .value


type alias Db =
    { people : Dict String (Person)
    , cars : Dict String (Car)
    }

type alias DbView =
    {
        people : Dict String PersonView,
        cars : Dict String CarView
    }

type alias DbBuilder =
    Dict String (PersonBuilder) -> Dict String CarBuilder -> Db


db1 : PartialUpdater Db (DbBuilder) Db
db1 = 
    entity Db
    |> substruct "people" (dict (\x -> Just x) string person) .people
    |> substruct "cars" (dict (\x -> Just x) string  car) .cars

type alias Timestamp a =
    { created : Int
    , modified : Int
    , accessed : Int
    , value : a
    }

timestamp : PartialUpdater a b a -> PartialUpdater (Timestamp a) (b -> Timestamp a) (Timestamp a)
timestamp other = 
    let
        t = 
            entity Timestamp
            |> attribute "created" int .created
            |> attribute "modified" int .modified
            |> attribute "accessed" int .accessed
            |> substruct "value" other .value
    in 
        {
            decoder = t.decoder,
            encoder = t.encoder,
            fuzzer = t.fuzzer,
            toString = t.toString,
            empty = t.empty,
            viewer = \db full -> Maybe.map (\x -> x.value ) (t.viewer db full)
        }