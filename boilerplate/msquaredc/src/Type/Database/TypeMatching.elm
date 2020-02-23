module Type.Database.TypeMatching exposing (..)

import Dict exposing (..)
import Type.Database exposing (..)
import Html exposing (Html, text)
import Type.IO.Form exposing (UpdateMsg(..))
type Type =
    AnswerType 
    | CoderType
    | CodingType
    | CodingAnswerType 
    | CodingFrameType
    | CodingQuestionType
    | CodingQuestionaryType
    | QuestionType
    | QuestionaryType
    | UserType

types : List Type
types = 
    [AnswerType 
    , CoderType
    , CodingType
    , CodingAnswerType 
    , CodingFrameType
    , CodingQuestionType
    , CodingQuestionaryType
    , QuestionType
    , QuestionaryType
    , UserType]

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
        "question" ->
            Just QuestionType
        "questionary" ->
            Just QuestionaryType
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
        QuestionType -> 
            "question"
        QuestionaryType ->
            "questionary"
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
        QuestionType -> 
            "questions"
        QuestionaryType ->
            "questionnaries"
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
        QuestionType ->
            question.fields
        QuestionaryType ->
            questionary.fields
        UserType ->
            user.fields

keys : Type -> Database -> List String
keys kind db =
    let
        g = Dict.keys
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
            QuestionType ->
                g db.questions
            QuestionaryType ->
                g db.questionnaries
            UserType ->
                g db.users

forms : String -> Type -> (UpdateMsg -> msg) -> String -> Type.Database.Database -> Maybe (List (Html.Html msg))
forms id kind m acc db =
    let
        g def table = Dict.get id table
                      |> Maybe.map (\x -> def.form id m x.value acc)
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
            QuestionType ->
                g question db.questions
            QuestionaryType ->
                g questionary db.questionnaries
            UserType ->
                g user db.users

new : String -> Type -> Database -> Database
new id kind db =
    let
        g table def update = Dict.insert id (timestamp def).empty table 
                            |> update db
    in
        case kind of
            AnswerType ->
                g db.answers answer (\t x -> {t | answers = x})
            CoderType ->
                g db.coders coder (\t x -> {t | coders = x})
            CodingType ->
                g db.codings coding (\t x -> {t | codings = x})
            CodingAnswerType ->
                g db.coding_answers coding_answer (\t x -> {t | coding_answers = x})
            CodingFrameType ->
                g db.coding_frames coding_frame (\t x -> {t | coding_frames = x})
            CodingQuestionType ->
                g db.coding_questions coding_question (\t x -> {t | coding_questions = x})
            CodingQuestionaryType ->
                g db.coding_questionnaries coding_questionary (\t x -> {t | coding_questionnaries = x})
            QuestionType ->
                g db.questions question (\t x -> {t | questions = x})
            QuestionaryType ->
                g db.questionnaries questionary (\t x -> {t | questionnaries = x})
            UserType ->
                g db.users user (\t x -> {t | users = x})

    
            
    
        
            
    
            
    
    

            
    
            
    