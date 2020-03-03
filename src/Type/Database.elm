module Type.Database exposing (..)

import Dict exposing (..)
import Fuzz
import Json.Decode
import Json.Encode
import Type.IO.Encoder exposing (Encoder(..))
import Type.IO exposing (..)
import Type.IO.Form as Form
import Type.Database.InputType as IT


--import Type.Database.User exposing (..)
--import Type.Database.User as User

type alias Table a =
    Dict String (Timestamp a)

type alias TableView a = 
    Dict String (TimestampView a)

table : IO a Database b msg -> IO (Table a) Database (TableView b) msg
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
    , events : Table Event
    , input_types : Table IT.InputType
    , places : Table Place
    , questions : Table Question
    , questionnaries : Table Questionary
    , studies : Table Study
    , test_subjects : Table TestSubject
    , users : Table User
    }

type alias DatabaseView =
    {
    answers : TableView AnswerView
    , coders : TableView CoderView
    , codings : TableView CodingView
    , coding_answers : TableView CodingAnswerView
    , coding_frames : TableView CodingFrameView
    , coding_questions : TableView CodingQuestionView
    , coding_questionnaries : TableView CodingQuestionaryView
    , events : TableView EventView
    , input_types : TableView IT.InputType
    , places : TableView Place
    , questions : TableView QuestionView
    , questionnaries : TableView QuestionaryView
    , studies : TableView StudyView
    , test_subjects : TableView TestSubject
    , users : TableView User
    }

database : IO Database Database DatabaseView msg
database =
    entity Database DatabaseView
        |> substruct "answers" (table answer) .answers
        |> substruct "coders" (table coder) .coders
        |> substruct "codings" (table coding) .codings
        |> substruct "coding_answers" (table coding_answer) .coding_answers
        |> substruct "coding_frames" (table coding_frame) .coding_frames
        |> substruct "coding_questions" (table coding_question) .coding_questions
        |> substruct "coding_questionnaries" (table coding_questionary) .coding_questionnaries
        |> substruct "events" (table event) .events
        |> substruct "input_types" (table IT.input_type) .input_types
        |> substruct "places" (table place) .places
        |> substruct "questions" (table question) .questions
        |> substruct "questionnaries" (table questionary) .questionnaries
        |> substruct "studies" (table study) .studies
        |> substruct "test_subjects" (table test_subject) .test_subjects
        |> substruct "users" (table user) .users


type alias Answer =
    { question : String
    , test_subject : String
    , value : String
    , event : String
    }

type alias AnswerView =
    {
        question : Question
        , test_subject : TestSubject
        , value : String
        , event : Event
    }


answer : IO Answer Database AnswerView msg
answer =
    entity Answer AnswerView
        |> reference "question" string .question .questions Dict.get .value
        |> reference "user" string .test_subject .test_subjects Dict.get .value
        |> attribute "value" string .value
        |> reference "event" string .event .events Dict.get .value


type alias Coder =
    { user : String
    }

type alias CoderView =
    {
        user : User
    }


coder : IO Coder Database CoderView msg
coder =
    entity Coder CoderView
        |> reference "user" string .user .users Dict.get .value


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

type alias Event = 
    {
        place : String,
        day : Int,
        study : String
    }

type alias EventView =
    {
        place : Place,
        day : Int,
        study : Study
    }

event : IO Event Database EventView msg
event =
    entity Event EventView
    |> reference "place" string .place .places Dict.get .value
    |> attribute "day" int .day
    |> reference "study" string .study .studies Dict.get .value

type alias Place =
    {
        name : String,
        latitude : Float,
        longitude : Float
    }

place : IO Place Database Place msg
place =
    entity Place Place
    |> attribute "name" string .name
    |> attribute "latitude" float .latitude
    |> attribute "longitude" float .longitude
    |> updateEmpty (\x -> {x | name = "Unnamed Place"})

type alias Question =
    { questionary : String
    , index : Int
    , text : String
    , input_type : IT.InputType
    }

type alias QuestionView =
    {
        questionary : Questionary
        , index : Int
        , text : String
        , input_type : IT.InputType
    }

question : IO Question Database QuestionView msg
question =
    entity Question QuestionView
        |> reference "questionary" string .questionary .questionnaries Dict.get .value
        |> attribute "index" int .index
        |> attribute "text" string .text
        |> attribute  "input_type" IT.input_type .input_type
        |> updateEmpty (\x -> {x | text = "Unnamed Question"})


type alias Questionary =
    { name : String,
    study : String
    }

type alias QuestionaryView =
    { name : String,
    study : Study
    }


questionary : IO Questionary Database QuestionaryView msg
questionary =
    entity Questionary QuestionaryView
        |> attribute "name" string .name
        |> reference "study" string .study .studies Dict.get .value
        |> updateEmpty (\x -> {x | name = "Unnamed Questionary "})

type alias Study =
    {
        name : String,
        description : String,
        leader : String
    }

type alias StudyView =
    {
        name : String,
        description : String,
        leader : User
    }


study : IO Study Database StudyView msg
study =
    
    entity Study StudyView
    |> attribute "name" string .name
    |> attribute "description" string .description
    |> reference "leader" string .leader .users Dict.get .value
    |> updateEmpty (\x -> {x | name = "Unnamed Study "})

type alias TestSubject =
    {
        infos : Dict String String
    }

test_subject : IO TestSubject Database TestSubject msg
test_subject =
    entity TestSubject TestSubject
    |> attribute "infos" (dict string string) .infos

type alias User =
    { name : Maybe String
    , email : Maybe String
    }


user : IO User Database User msg
user =
    entity User User
        |> attribute "name" (maybe string) .name
        |> attribute "email" (maybe string) .email


type alias Timestamp a =
    { creator : String
    , created : Int
    , modified : Int
    , accessed : Int
    , value : a
    }

type alias TimestampView a =
    { creator : User
    , created : Int
    , modified : Int
    , accessed : Int
    , value : a
    }

timestamp : IO a Database b msg -> IO (Timestamp a) Database (TimestampView b) msg
timestamp other = 
    let
        t = 
            entity Timestamp TimestampView
            |> reference "creator" string .creator .users Dict.get .value
            |> attribute "created" int .created
            |> attribute "modified" int .modified
            |> attribute "accessed" int .accessed
            |> substruct "value" other .value
    in 
        t
        -- {
        --     decoder = t.decoder,
        --     encoder = t.encoder,
        --     fuzzer = t.fuzzer,
        --     toString = t.toString,
        --     empty = t.empty,
        --     viewer = \db full -> Maybe.map (\x -> x.value ) (t.viewer db full),
        --     fields = t.fields,
        --     form = \name callback value acc -> t.form name callback value ("value." ++acc),
        --     updater = t.updater,
        --     strDecoder = t.strDecoder
        -- }

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
    | StudyType
    | UserType
    | EventType

updateEmpty : (a -> a) -> IO a b c msg -> IO a b c msg
updateEmpty f prev =
    {prev | empty = f prev.empty}