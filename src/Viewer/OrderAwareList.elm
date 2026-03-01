module Viewer.OrderAwareList exposing (OrderAware, orderAwareList)

import Type.IO.Internal exposing (Id)


type alias OrderAware a =
    { value : a
    , previous :
        Maybe
            { id : Id a String
            , value : a
            }
    , next :
        Maybe
            { id : Id a String
            , value : a
            }
    , id : Id a String
    }


prePost : Maybe a -> List a -> List ( Maybe a, a, Maybe a )
prePost prev xs =
    case xs of
        [] ->
            []

        a :: [] ->
            [ ( prev, a, Nothing ) ]

        a :: b :: c ->
            ( prev, a, Just b ) :: prePost (Just a) (b :: c)


orderAwareList : List ( Id a String, a ) -> List (OrderAware a)
orderAwareList old =
    let
        mapToValue : Maybe ( Id a String, a ) -> Maybe { id : Id a String, value : a }
        mapToValue a =
            case a of
                Just ( id, val ) ->
                    Just { id = id, value = val }

                Nothing ->
                    Nothing
    in
    prePost Nothing old
        |> List.map (\( x, ( id, value ), y ) -> { value = value, id = id, previous = mapToValue x, next = mapToValue y })
