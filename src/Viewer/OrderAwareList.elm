module Viewer.OrderAwareList exposing (..)


type alias OrderAware a =
    { value : a
    , previous :
        Maybe
            { id : String
            , value : a
            }
    , next :
        Maybe
            { id : String
            , value : a
            }
    , id : String
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


orderAwareList : List ( String, a ) -> List (OrderAware a)
orderAwareList old =
    let
        mapToValue a =
            case a of
                Just ( id, val ) ->
                    Just { id = id, value = val }

                Nothing ->
                    Nothing
    in
    prePost Nothing old
        |> List.map (\( x, ( id, value ), y ) -> { value = value, id = id, previous = mapToValue x, next = mapToValue y })
