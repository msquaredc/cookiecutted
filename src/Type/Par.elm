module Type.Par exposing (Par(..), Parf(..), SParf(..), andThen, apply, applyFirst, applySecond, distribute, fromTuple, inverseMapF, linearize, map, mapTuple, myMap, toTuple)


type Par a
    = Par ( a, a )


mapTuple : (( a, a ) -> ( b, b )) -> Par a -> Par b
mapTuple f (Par x) =
    Par (f x)


fromTuple : ( a, a ) -> Par a
fromTuple =
    Par


toTuple : Par a -> ( a, a )
toTuple (Par x) =
    x


andThen : (( a, a ) -> Par b) -> Par a -> Par b
andThen f (Par x) =
    f x


distribute : Par (a -> b) -> Par a -> Par b
distribute (Par ( f, g )) (Par ( x, y )) =
    Par ( f x, g y )


inverseMapF : (Par a -> Par b) -> Par (a -> b)
inverseMapF f =
    Debug.todo ""


map : (Par a -> Par b) -> Par a -> Par b
map f x =
    f x


myMap : Par ((a -> a) -> b) -> (Par a -> Par a) -> Par b
myMap =
    Debug.todo ""


type Parf a
    = Parf (( a, a ) -> ( a, a ))



--  Parf (Par a -> Par a)


apply : Parf a -> Par a -> Par a
apply (Parf f) (Par x) =
    Par (f x)


applyFirst : Parf a -> Par a -> a
applyFirst b c =
    apply b c
        |> (\(Par ( a, _ )) -> a)


applySecond : Parf a -> Par a -> a
applySecond b c =
    apply b c
        |> (\(Par ( _, a )) -> a)


type SParf a
    = SParf (( a, a ) -> a)


linearize : SParf a -> a -> a -> a
linearize (SParf f) x y =
    f ( x, y )
