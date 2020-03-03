module Type.Database.Description exposing (..)

import Dict

type Entity a = 
    Entity
        {
            value : a
        }

type Description a =
    Description (a -> Entity a)

entityMap : (a->b) -> Entity a -> Entity b
entityMap f (Entity entity) = 
    Entity {value = (f entity.value)}

custom : (a -> Entity a) -> Description a
custom f = 
    Description f

-- optional : String -> Description a -> a -> Description (a -> b) -> Description b
-- optional key valDescription fallback decoder = 

map : (a -> b) -> Description a -> Description b
map f (Description desc) = 
    Description (f desc)


string : Description String
string =
    Description (\x -> Entity {value = x})




-- type alias Description a =
--     {
--         int : Dict.Dict String (a -> Int),
--         float : Dict.Dict String (a -> Float),
--         string : Dict.Dict String (a -> String)
--     }

-- describe : Description a
-- describe =
--     {
--         int = Dict.empty,
--         float = Dict.empty,
--         string = Dict.empty
--     }

-- int : String -> (a -> Int ) -> Description a -> Description a
-- int name mapper old = 
--     {old | int = Dict.insert name mapper old.int}

-- float : String -> (a -> Float ) -> Description a -> Description a
-- float name mapper old = 
--     {old | float = Dict.insert name mapper old.float}

-- string : String -> (a -> String ) -> Description a -> Description a
-- string name mapper old = 
--     {old | string = Dict.insert name mapper old.string}

-- maybe : String -> (a -> Maybe String) -> Description a -> Description a
-- maybe 