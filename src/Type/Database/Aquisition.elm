module Type.Database.Aquisition exposing (..)

import Dict exposing (Dict)
import Type.Database as Db exposing (Database, Row, Table, Timestamp, Type(..))
import Type.Database.TypeMatching as Match
import Type.IO.Internal as Id exposing (Id)
import Type.Database exposing (coding_questionary)


type AttributeAccessor a b
    = Raw (Row a -> b)
    | Value (a -> b)



type alias Aquisition a b c =
    { receiver : a
    , reference : Id b c
    }


type alias SerializableStudyDatapoint =
    { event : String
    , question : String
    , answer : String
    , coding_question : String
    , coding_answer : String
    , coder : String
    }


serializableStudyDatapoint : String -> Database -> List (Aquisition SerializableStudyDatapoint Type.Database.CodingQuestion String)
serializableStudyDatapoint id db =
    Aquisition SerializableStudyDatapoint (Id.box id)
        |> addAttrSingle (Value .study) .events (Value .name) db
        |> moveReferenceList (Value .study) .questionnaries (Raw Tuple.first) db
        |> addAttrList (Value .questionary) .questions (Value .text) db
        -- |> addAttrList (Value .questionary) .questions (Value .input_ty) db (\_ -> ["Implement Me!"])
        |> moveReferenceList (Value .questionary) .questions (Raw Tuple.first) db
        |> addAttrList (Value .question) .answers (Value .value) db
        --|> moveReferenceList (Value .question) .answers (Raw Tuple.first) db
        |> moveReferenceList (Value .question) .coding_questionnaries (Raw Tuple.first) db
        |> addAttrList (Value .coding_questionary) .coding_questions (Value .text) db
        |> moveReferenceList (Value .coding_questionary) .coding_questions (Raw Tuple.first) db
        |> addAttrList (Value .coding_question) .coding_answers (Value .value) db
        |> moveReferenceList (Value .coding_question) .coding_answers (Raw Tuple.first) db 
        --|> moveReferenceList (Raw (\(x,y) -> y.creator)) .users (Raw Tuple.first) db
        |> addAttrList (Raw (\(x,y) -> y.creator)) .users (Value (\x -> Maybe.withDefault "" x.name)) db
        



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


updateReference : Aquisition a b d -> Id c e -> (Aquisition a c e)
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



addAttrList : AttributeAccessor c (Id d e) -> (Database -> Table c) -> AttributeAccessor c a -> Database -> List (Aquisition (a -> b) d e) -> List (Aquisition b d e)
addAttrList attr dbget selectvalue db aquisitions =
    List.concatMap (addAttrSingle attr dbget selectvalue db) aquisitions


addAttrSingle :
    AttributeAccessor c (Id d e) 
    -> (Database -> Table c)
    -> AttributeAccessor c a
    -> Database
    -> Aquisition (a -> b) d e
    -> List (Aquisition b d e)
addAttrSingle attr dbget selectvalue db aquisition =
    let
        attrf =
            transformAccessor attr

        selectf =
            transformAccessor selectvalue

    in
    Match.filterBy attrf dbget db aquisition.reference
        |> List.map selectf
        |> List.map (updateReciever aquisition)


moveReferenceList : AttributeAccessor c (Id a f) -> (Database -> Table c) -> AttributeAccessor c (Id b e) -> Database -> List (Aquisition d a f) -> List (Aquisition d b e)
moveReferenceList attr dbget selectvalue db aquisitions =
    List.concatMap (moveReferenceSingle attr dbget selectvalue db) aquisitions


moveReferenceSingle :
    AttributeAccessor c (Id a f)
    -> (Database -> Table c)
    -> AttributeAccessor c (Id b e)
    -> Database
    -> Aquisition d a f
    -> List (Aquisition d b e)
moveReferenceSingle attr dbget selectvalue db aquisition =
    let
        attrf =
            transformAccessor attr

        selectf =
            transformAccessor selectvalue

    in
        Match.filterBy attrf dbget db aquisition.reference
        |> List.map selectf
        |> List.map (updateReference aquisition)

