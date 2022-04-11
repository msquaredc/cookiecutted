module Type.Database.Aquisition exposing (Aquisition, AttributeAccessor(..), add, aquire, end, move, moveReferenceSingle, start)

import Type.Database as Db exposing (Row, Table)
import Type.IO.Internal exposing (Id)


type AttributeAccessor a b
    = Raw (Row a -> b)
    | Value (a -> b)


type alias Aquisition a b c =
    { receiver : a
    , reference : Id b c
    }


start :
    AttributeAccessor c (Id d e)
    -> Table c
    -> AttributeAccessor c a
    -> Aquisition (a -> b) d e
    -> List (Aquisition b d e)
start =
    addAttrSingle


add : AttributeAccessor c (Id d e) -> Table c -> AttributeAccessor c a -> List (Aquisition (a -> b) d e) -> List (Aquisition b d e)
add =
    addAttrList


move : AttributeAccessor c (Id a f) -> Table c -> AttributeAccessor c (Id b e) -> List (Aquisition d a f) -> List (Aquisition d b e)
move =
    moveReferenceList


end : List (Aquisition a b c) -> List a
end =
    aquire



{- |> addAttrList (Value .answer) .coding_question (Value .text) db identity
   |> moveReferenceList (Value .answer) .coding_questions Id db identity
   |> addAttrList (Value )
-}
-- |> aquire
-- let
--     events = Match.filterBy (\(_,x) -> x.value.study) .events db id
-- in
--     List.map (\(eid,eval) -> (eid, SerializableStudyDatapoint eval.value.name)) events


updateReciever : Aquisition (a -> c) b d -> a -> Aquisition c b d
updateReciever { receiver, reference } val =
    { receiver = receiver val
    , reference = reference
    }


updateReference : Aquisition a b d -> Id c e -> Aquisition a c e
updateReference { receiver, reference } val =
    { receiver = receiver, reference = val }


aquire : List (Aquisition a b c) -> List a
aquire l =
    List.map .receiver l


transformAccessor : AttributeAccessor a b -> (Row a -> b)
transformAccessor accessor =
    case accessor of
        Value f ->
            \( _, x ) -> f x.value

        Raw f ->
            f


addAttrList : AttributeAccessor c (Id d e) -> Table c -> AttributeAccessor c a -> List (Aquisition (a -> b) d e) -> List (Aquisition b d e)
addAttrList attr table selectvalue aquisitions =
    List.concatMap (addAttrSingle attr table selectvalue) aquisitions


addAttrSingle :
    AttributeAccessor c (Id d e)
    -> Table c
    -> AttributeAccessor c a
    -> Aquisition (a -> b) d e
    -> List (Aquisition b d e)
addAttrSingle attr table selectvalue aquisition =
    let
        attrf : Row c -> Id d e
        attrf =
            transformAccessor attr

        selectf : Row c -> a
        selectf =
            transformAccessor selectvalue
    in
    filterBy attrf table aquisition.reference
        |> List.map selectf
        |> List.map (updateReciever aquisition)


filterBy : (Row a -> Id b c) -> Table a -> Id b c -> List (Row a)
filterBy attr table old =
    table
        |> Db.rows
        |> List.filter (\x -> attr x == old)


moveReferenceList : AttributeAccessor c (Id a f) -> Table c -> AttributeAccessor c (Id b e) -> List (Aquisition d a f) -> List (Aquisition d b e)
moveReferenceList attr table selectvalue aquisitions =
    List.concatMap (moveReferenceSingle attr table selectvalue) aquisitions


moveReferenceSingle :
    AttributeAccessor c (Id a f)
    -> Table c
    -> AttributeAccessor c (Id b e)
    -> Aquisition d a f
    -> List (Aquisition d b e)
moveReferenceSingle attr table selectvalue aquisition =
    let
        attrf : Row c -> Id a f
        attrf =
            transformAccessor attr

        selectf : Row c -> Id b e
        selectf =
            transformAccessor selectvalue
    in
    filterBy attrf table aquisition.reference
        |> List.map selectf
        |> List.map (updateReference aquisition)
