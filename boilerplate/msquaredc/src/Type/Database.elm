module Type.Database exposing (..)

import Dict exposing (..)
import Fuzz
import Json.Decode
import Json.Encode
import Type.IO.Encoder exposing (Encoder(..))
import Type.IO exposing (..)


--import Type.Database.User exposing (..)
--import Type.Database.User as User

type alias Table a =
    Dict String (Timestamp a)

type alias TableView a = 
    Dict String a

table : IO a db b msg -> IO (Table a) db (TableView b) msg
table kind = 
    dict string (timestamp kind)


type alias Database =
    { answers : Table Answer
    , coders : Table Coder
    , codings : Table Coding
    , coding_answers : Table CodingAnswer
    , coding_frames : Table CodingFrame
    , coding_questions : Table CodingQuestion
    , coding_questionnaries : Table CodingQuestionary
    , questions : Table Question
    , questionnaries : Table Questionary
    , users : Table User
    }

type alias DatabaseView =
    {
    answers : TableView AnswerView
    , coders : TableView Coder
    , codings : TableView CodingView
    , coding_answers : TableView CodingAnswerView
    , coding_frames : TableView CodingFrameView
    , coding_questions : TableView CodingQuestionView
    , coding_questionnaries : TableView CodingQuestionaryView
    , questions : TableView QuestionView
    , questionnaries : TableView Questionary
    , users : TableView User
    }


database =
    entity Database DatabaseView
        |> substruct "answer" (table answer) .answers
        |> substruct "coder" (table coder) .coders
        |> substruct "coding" (table coding) .codings
        |> substruct "coding_answer" (table coding_answer) .coding_answers
        |> substruct "coding_frame" (table coding_frame) .coding_frames
        |> substruct "coding_question" (table coding_question) .coding_questions
        |> substruct "coding_questionnary" (table coding_questionary) .coding_questionnaries
        |> substruct "question" (table question) .questions
        |> substruct "questionnaries" (table questionary) .questionnaries
        |> substruct "users" (table user) .users


type alias Answer =
    { question : String
    , user : String
    , value : String
    }

type alias AnswerView =
    {
        question : Question
        , user : User
        , value : String
    }


answer : IO Answer Database AnswerView msg
answer =
    entity Answer AnswerView
        |> reference "question" string .question .questions Dict.get .value
        |> reference "user" string .user .users Dict.get .value
        |> attribute "value" string .value


type alias Coder =
    { name : String
    }


coder : IO Coder Database Coder msg
coder =
    entity Coder Coder
        |> attribute "name" string .name


type alias Coding =
    { coder : String
    }

type alias CodingView  =
    {
        coder : Coder
    }

coding : IO Coding Database CodingView msg
coding =
    entity Coding CodingView
        |> reference "coder" string .coder .coders Dict.get .value


type alias CodingAnswer =
    { coding_question : String
    , coding_frame : String
    , value : String
    }

type alias CodingAnswerView = 
    {
        coding_question : CodingQuestion
        , coding_frame : CodingFrame
        , value : String
    }


coding_answer : IO CodingAnswer Database CodingAnswerView msg
coding_answer =
    entity CodingAnswer CodingAnswerView
        |> reference "coding_question" string .coding_question .coding_questions Dict.get .value
        |> reference "coding_frame" string .coding_frame .coding_frames Dict.get .value
        |> attribute "value" string .value


type alias CodingFrame =
    {
        answer : String,
        coding : String
    }

type alias CodingFrameView =
    {
        answer : Answer
        , coding : Coding
    }

coding_frame : IO CodingFrame Database CodingFrameView msg
coding_frame =
    entity CodingFrame CodingFrameView
    |> reference "answer" string .answer .answers Dict.get .value
    |> reference "coding" string .coding .codings Dict.get .value

type alias CodingQuestion =
    {
        coding_questionary : String
    }

type alias CodingQuestionView =
    {
        coding_questionary : CodingQuestionary
    }

coding_question : IO CodingQuestion Database CodingQuestionView msg
coding_question =
    entity CodingQuestion CodingQuestionView
        |> reference "coding_questionary" string .coding_questionary .coding_questionnaries Dict.get .value

type alias CodingQuestionary = 
    {
        question : String
    }

type alias CodingQuestionaryView =
    {
        question : Question
    }

coding_questionary : IO CodingQuestionary Database CodingQuestionaryView msg
coding_questionary = 
    entity CodingQuestionary CodingQuestionaryView
        |> reference "question" string .question .questions Dict.get .value

type alias Question =
    { questionary : String
    , text : String
    , input_type : InputType
    }

type alias QuestionView =
    {
        questionary : Questionary
        , text : String
        , input_type : InputType
    }

question : IO Question Database QuestionView msg
question =
    entity Question QuestionView
        |> reference "questionary" string .questionary .questionnaries Dict.get .value
        |> attribute "text" string .text
        |> attribute "input_type" input_type .input_type


type alias Questionary =
    { name : String
    }


questionary : IO Questionary Database Questionary msg
questionary =
    entity Questionary Questionary
        |> attribute "name" string .name


type alias User =
    { infos : Dict String String
    }


user : IO User Database User msg
user =
    entity User User
        |> attribute "infos" (dict string string) .infos


type InputType
    = YesNo


input_type : IO InputType db InputType msg
input_type =
    { decoder = Json.Decode.succeed YesNo
    , strDecoder = \_ -> Json.Decode.succeed YesNo
    , encoder = SingleEncoder (\_ -> Json.Encode.string "bool")
    , fuzzer = Fuzz.constant YesNo
    , toString = \_ _ -> Ok "Bool"
    , viewer = \_ full -> Just full
    , empty = YesNo
    , fields = []
    , form = \_ _ _ _ -> []
    , updater = \_ a -> Ok a
    }

type alias Timestamp a =
    { created : Int
    , modified : Int
    , accessed : Int
    , value : a
    }

timestamp : IO a db b msg -> IO (Timestamp a) db b msg
timestamp other = 
    let
        t = 
            entity Timestamp Timestamp
            |> attribute "created" int .created
            |> attribute "modified" int .modified
            |> attribute "accessed" int .accessed
            |> substruct "value" other .value
    in 
        {
            decoder = t.decoder,
            encoder = t.encoder,
            fuzzer = t.fuzzer,
            toString = t.toString,
            empty = t.empty,
            viewer = \db full -> Maybe.map (\x -> x.value ) (t.viewer db full),
            fields = t.fields,
            form = \name callback value acc -> t.form name callback value ("value." ++acc),
            updater = t.updater,
            strDecoder = t.strDecoder
        }

