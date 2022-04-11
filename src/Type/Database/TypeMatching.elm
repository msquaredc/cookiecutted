module Type.Database.TypeMatching exposing (DispatchType(..), FieldConfig, FieldUpdateConfig, concatTupleFirst, concatTupleLast, delete, dispatchDb, fields, filterBy, forms, fromString, getField, getTimestampUpdaterMsg, join, keys, new, resolveAttributes, setField, setFieldRaw, setManyFields, setTimestamp, swapFields, toString, toStringPlural, types, updateField)

import Dict exposing (..)
import Html exposing (Html)
import Msg
import Task exposing (perform)
import Time exposing (Posix, now, posixToMillis)
import Type.Database as Db exposing (..)
import Type.Database.InputType as IT exposing (InputType, input_type)
import Type.IO.Form as Form exposing (UpdateMsg(..))
import Type.IO.Internal as Id exposing (Id, unbox)
import Type.IO.Setter as Updater


types : List Type
types =
    [ AnswerType
    , CoderType
    , CodingType
    , CodingAnswerType
    , CodingFrameType
    , CodingQuestionType
    , CodingQuestionaryType
    , EventType
    , QuestionType
    , QuestionaryType
    , StudyType
    , UserType
    , InputTypeType ShortKind
    , InputTypeType ListKind
    , InputTypeType LongKind
    ]


fromString : String -> Maybe Type
fromString name =
    case name of
        "answer" ->
            Just AnswerType

        "coder" ->
            Just CoderType

        "coding" ->
            Just CodingType

        "coding_answer" ->
            Just CodingAnswerType

        "coding_frame" ->
            Just CodingFrameType

        "coding_question" ->
            Just CodingQuestionType

        "coding_questionary" ->
            Just CodingQuestionaryType

        "event" ->
            Just EventType

        "question" ->
            Just QuestionType

        "questionary" ->
            Just QuestionaryType

        "study" ->
            Just StudyType

        "user" ->
            Just UserType

        "input_type" ->
            Just (InputTypeType ShortKind)

        "test_subject" ->
            Just TestSubjectType

        _ ->
            Nothing


toString : Type -> String
toString kind =
    case kind of
        AnswerType ->
            "answer"

        CoderType ->
            "coder"

        CodingType ->
            "coding"

        CodingAnswerType ->
            "coding_answer"

        CodingFrameType ->
            "coding_frame"

        CodingQuestionType ->
            "coding_question"

        CodingQuestionaryType ->
            "coding_questionary"

        EventType ->
            "event"

        QuestionType ->
            "question"

        QuestionaryType ->
            "questionary"

        StudyType ->
            "study"

        UserType ->
            "user"

        TestSubjectType ->
            "test_subject"

        InputTypeType _ ->
            "input_type"


toStringPlural : Type -> String
toStringPlural kind =
    case kind of
        AnswerType ->
            "answers"

        CoderType ->
            "coders"

        CodingType ->
            "codings"

        CodingAnswerType ->
            "coding_answers"

        CodingFrameType ->
            "coding_frames"

        CodingQuestionType ->
            "coding_questions"

        CodingQuestionaryType ->
            "coding_questionnaries"

        EventType ->
            "events"

        QuestionType ->
            "questions"

        QuestionaryType ->
            "questionnaries"

        StudyType ->
            "studies"

        UserType ->
            "users"

        TestSubjectType ->
            "test_subjects"

        InputTypeType _ ->
            "input_types"


fields : Type -> List String
fields kind =
    case kind of
        AnswerType ->
            answer.fields

        CoderType ->
            coder.fields

        CodingType ->
            coding.fields

        CodingAnswerType ->
            coding_answer.fields

        CodingFrameType ->
            coding_frame.fields

        CodingQuestionType ->
            coding_question.fields

        CodingQuestionaryType ->
            coding_questionary.fields

        EventType ->
            event.fields

        QuestionType ->
            question.fields

        QuestionaryType ->
            questionary.fields

        StudyType ->
            study.fields

        UserType ->
            user.fields

        TestSubjectType ->
            test_subject.fields

        InputTypeType _ ->
            input_type.fields


keys : Type -> Database -> List String
keys kind db =
    let
        g =
            Dict.keys
    in
    case kind of
        AnswerType ->
            g db.answers

        CoderType ->
            g db.coders

        CodingType ->
            g db.codings

        CodingAnswerType ->
            g db.coding_answers

        CodingFrameType ->
            g db.coding_frames

        CodingQuestionType ->
            g db.coding_questions

        CodingQuestionaryType ->
            g db.coding_questionnaries

        EventType ->
            g db.events

        QuestionType ->
            g db.questions

        QuestionaryType ->
            g db.questionnaries

        StudyType ->
            g db.studies

        UserType ->
            g db.users

        TestSubjectType ->
            g db.test_subjects

        InputTypeType _ ->
            g db.input_types


forms : String -> Type -> String -> Db.Database -> (String -> (String -> Msg.Msg) -> Html Msg.Msg) -> Result Form.Error (Html.Html Msg.Msg)
forms id kind acc db f =
    let
        m : UpdateMsg -> Msg.Msg
        m =
            \x ->
                Msg.Form <|
                    Form.AttrMsg (toStringPlural kind) <|
                        Form.DictMsg (Just id) <|
                            Form.AttrMsg "value" x

        g def table =
            Dict.get id table
                |> Maybe.map (\x -> def.form id m x.value acc f)
                |> Maybe.withDefault (Err Form.NotFound)
    in
    case kind of
        AnswerType ->
            g answer db.answers

        CoderType ->
            g coder db.coders

        CodingType ->
            g coding db.codings

        CodingAnswerType ->
            g coding_answer db.coding_answers

        CodingFrameType ->
            g coding_frame db.coding_frames

        CodingQuestionType ->
            g coding_question db.coding_questions

        CodingQuestionaryType ->
            g coding_questionary db.coding_questionnaries

        EventType ->
            g event db.events

        QuestionType ->
            g question db.questions

        QuestionaryType ->
            g questionary db.questionnaries

        StudyType ->
            g study db.studies

        UserType ->
            g user db.users

        TestSubjectType ->
            g test_subject db.test_subjects

        InputTypeType _ ->
            g input_type db.input_types


type DispatchType
    = New (Id Db.User String)
    | Delete


new : Id a String -> Type -> Id Db.User String -> Database -> Database
new id kind u db =
    dispatchDb (New u) id kind db


delete : Id a String -> Type -> Database -> Database
delete =
    dispatchDb Delete


dispatchDb : DispatchType -> Id a String -> Type -> Database -> Database
dispatchDb dt id kind db =
    let
        g table def update =
            update db <|
                case dt of
                    New u ->
                        let
                            config =
                                (timestamp def).empty
                        in
                        Dict.insert (unbox id) { config | creator = u } table

                    Delete ->
                        Dict.remove (unbox id) table
    in
    case kind of
        AnswerType ->
            g db.answers answer (\t x -> { t | answers = x })

        CoderType ->
            g db.coders coder (\t x -> { t | coders = x })

        CodingType ->
            g db.codings coding (\t x -> { t | codings = x })

        CodingAnswerType ->
            g db.coding_answers coding_answer (\t x -> { t | coding_answers = x })

        CodingFrameType ->
            g db.coding_frames coding_frame (\t x -> { t | coding_frames = x })

        CodingQuestionType ->
            g db.coding_questions coding_question (\t x -> { t | coding_questions = x })

        CodingQuestionaryType ->
            g db.coding_questionnaries coding_questionary (\t x -> { t | coding_questionnaries = x })

        EventType ->
            g db.events event (\t x -> { t | events = x })

        QuestionType ->
            g db.questions question (\t x -> { t | questions = x })

        QuestionaryType ->
            g db.questionnaries questionary (\t x -> { t | questionnaries = x })

        StudyType ->
            g db.studies study (\t x -> { t | studies = x })

        UserType ->
            g db.users user (\t x -> { t | users = x })

        TestSubjectType ->
            g db.test_subjects test_subject (\t x -> { t | test_subjects = x })

        InputTypeType it ->
            case it of
                ShortKind ->
                    g db.input_types input_type (\t x -> { t | input_types = x })

                LongKind ->
                    g db.input_types { input_type | empty = IT.LongAnswer IT.longAnswerConfig.empty } (\t x -> { t | input_types = x })

                ListKind ->
                    g db.input_types { input_type | empty = IT.List IT.listConfig.empty } (\t x -> { t | input_types = x })



{- getReferenceHolder : (Type, String) -> Database -> List (Type, String)
   getReferenceHolder (kind,id) db =
       case kind of
           AnswerType ->
               CodingAnswer
-}


getField : String -> String -> Type -> Database -> Maybe String
getField id fname kind db =
    database.toString (toStringPlural kind ++ "." ++ id ++ ".value." ++ fname) db
        |> Result.toMaybe


getTimestampUpdaterMsg : Type -> String -> String -> Posix -> Msg.Msg
getTimestampUpdaterMsg kind id attribute time =
    Msg.CRUD <|
        Msg.Update <|
            Updater.AttributeMsg (toStringPlural kind) <|
                Updater.DictKeyMsg id <|
                    Updater.AttributeMsg attribute <|
                        Updater.IntMsg <|
                            posixToMillis time


setTimestamp : Type -> String -> String -> Cmd Msg.Msg
setTimestamp kind id attribute =
    getTimestampUpdaterMsg kind id attribute
        |> (\x -> perform x now)


filterBy : (Row b -> Id a c) -> (Database -> Table b) -> Database -> Id a c -> List (Row b)
filterBy attr dbgetter db old =
    dbgetter db
        |> Db.rows
        |> List.filter (\x -> attr x == old)


resolveAttributes : (a -> Id b String) -> (Database -> Table b) -> Database -> Row a -> List ( Row a, Row b )
resolveAttributes attr dbgetter db ( oldid, fullold ) =
    let
        f id =
            dbgetter db
                |> Db.rows
                |> List.filter (\( cid, _ ) -> cid == id)
    in
    ( oldid, fullold )
        |> (\( id, value ) -> ( ( id, value ), f (attr value.value) ))
        |> (\( oldval, list ) -> List.map (\newval -> ( oldval, newval )) list)


join : (Row b -> Id a String) -> (Database -> Table b) -> Database -> List (Row a) -> List ( Row a, Row b )
join attr dbgetter db old =
    let
        k =
            List.map (\( id, value ) -> id) old
    in
    old
        |> List.map (\( id, value ) -> ( ( id, value ), filterBy attr dbgetter db id ))
        |> List.map (\( oldval, list ) -> List.map (\newval -> ( oldval, newval )) list)
        |> List.concat


concatTupleFirst : ( List a, b ) -> List ( a, b )
concatTupleFirst ( l, elem ) =
    List.map (\x -> ( x, elem )) l


concatTupleLast : ( a, List b ) -> List ( a, b )
concatTupleLast ( elem, l ) =
    List.map (\x -> ( elem, x )) l


type alias FieldConfig a b =
    { kind : Type
    , attribute : String
    , setter : a -> Updater.Msg
    , id : Id b String
    , value : a
    }


setField : FieldConfig a b -> Msg.Msg
setField { kind, attribute, setter, id, value } =
    Msg.CRUD <|
        Msg.Update <|
            Updater.AttributeMsg (toStringPlural kind) <|
                Updater.DictKeyMsg (unbox id) <|
                    Updater.AttributeMsg "value" <|
                        Updater.AttributeMsg attribute <|
                            setter value


setManyFields : List (FieldConfig a b) -> Msg.Msg
setManyFields f =
    List.map setFieldRaw f
        |> Msg.UpdateAll
        |> Msg.CRUD


setFieldRaw : FieldConfig a b -> Updater.Msg
setFieldRaw { kind, attribute, setter, id, value } =
    Updater.AttributeMsg (toStringPlural kind) <|
        Updater.DictKeyMsg (unbox id) <|
            Updater.AttributeMsg "value" <|
                Updater.AttributeMsg attribute <|
                    setter value


type alias FieldUpdateConfig a =
    { kind : Type
    , attribute : String
    , setter : (a -> a) -> Updater.Msg
    , id : String
    }


updateField : FieldUpdateConfig a -> (a -> a) -> Updater.Msg
updateField config updater =
    Updater.AttributeMsg (toStringPlural config.kind) <|
        Updater.DictKeyMsg config.id <|
            Updater.AttributeMsg "value" <|
                Updater.AttributeMsg config.attribute <|
                    config.setter updater



-- go down and get value via update


swapFields : Type -> String -> (a -> Updater.Msg) -> ( Id b String, Id b String ) -> ( a, a ) -> Msg.Msg
swapFields kind attribute setter ( f_id, s_id ) ( f_val, s_val ) =
    Msg.CRUD <|
        Msg.UpdateAll
            [ setFieldRaw
                { kind = kind, attribute = attribute, setter = setter, id = f_id, value = s_val }
            , setFieldRaw
                { kind = kind, attribute = attribute, setter = setter, id = s_id, value = f_val }
            ]



-- swapFields : FieldUpdateConfig a -> FieldUpdateConfig a -> Database -> Database
-- swapFields first second db =
--     let
--         firstMsg x = database.updater (updateField first x) db
--         secondMsg y = database.updater (updateField second y) db
--     in
--         Tuple.mapBoth (\x -> database.updater (firstMsg x) db) (\y -> database.updater (secondMsg y) db)
