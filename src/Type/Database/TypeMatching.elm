module Type.Database.TypeMatching exposing (..)

import Dict exposing (..)
import Html exposing (Html)
import Msg
import Task exposing (perform)
import Time exposing (Posix, now, posixToMillis)
import Type.Database exposing (..)
import Type.IO.Form as Form exposing (UpdateMsg(..))
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


forms : String -> Type -> String -> Type.Database.Database -> (String -> (String -> Msg.Msg) -> Html Msg.Msg) -> Result Form.Error (Html.Html Msg.Msg)
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


new : String -> Type -> String -> Database -> Database
new id kind u db =
    let
        g table def update =
            let
                config =
                    (timestamp def).empty
            in
            Dict.insert id { config | creator = u } table
                |> update db
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


getField : String -> String -> Type -> Database -> Maybe String
getField id fname kind db =
    database.toString (toString kind ++ "." ++ id ++ ".value." ++ fname) db
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


type alias FieldConfig a =
    {
        kind : Type,
        attribute : String,
        setter : (a -> Updater.Msg),
        id : String,
        value : a
    }

setField : FieldConfig a -> Msg.Msg
setField {kind, attribute, setter, id, value} =
    Msg.CRUD <|
        Msg.Update <|
            Updater.AttributeMsg (toStringPlural kind) <|
                Updater.DictKeyMsg id <|
                    Updater.AttributeMsg "value" <|
                        Updater.AttributeMsg attribute <|
                            setter value

type alias FieldUpdateConfig a =
    {
        kind : Type,
        attribute : String,
        setter : ((a -> a) -> Updater.Msg),
        id : String
    }

updateField : FieldUpdateConfig a -> (a -> a)-> Updater.Msg
updateField config updater =
    Updater.AttributeMsg (toStringPlural config.kind) <|
        Updater.DictKeyMsg config.id <|
            Updater.AttributeMsg "value" <|
                Updater.AttributeMsg config.attribute <|
                    config.setter updater

-- swapFields : FieldUpdateConfig a -> FieldUpdateConfig a -> Database -> Database
-- swapFields first second db = 
--     let
--         firstMsg x = database.updater (updateField first x) db
--         secondMsg y = database.updater (updateField second y) db
--     in
--         Tuple.mapBoth (\x -> database.updater (firstMsg x) db) (\y -> database.updater (secondMsg y) db)
