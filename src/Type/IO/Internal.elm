module Type.IO.Internal exposing (Id, box, unbox)

type Id a b =
    Id b

box : c -> Id a c
box = Id

unbox : Id a c -> c
unbox (Id v) = v