module Type.Database exposing (Answer, AnswerView, Coder, CoderView, Coding, CodingAnswer, CodingAnswerView, CodingFrame, CodingFrameView, CodingQuestion, CodingQuestionView, CodingQuestionary, CodingQuestionaryView, CodingView, Database, DatabaseView, Event, EventView, InputTypeKind(..), Place, Question, QuestionView, Questionary, QuestionaryView, Row, Study, StudyView, Table, TableView, TestSubject, TestSubjectView, Timestamp, TimestampView, Type(..), User, answer, coder, coding, coding_answer, coding_frame, coding_question, coding_questionary, database, event, place, question, questionary, rows, study, table, test_subject, timestamp, updateEmpty, user)

import Dict exposing (..)
import Tuple
import Type.Database.InputType as IT
import Type.IO exposing (..)
import Type.IO.Encoder exposing (Encoder(..))
import Type.IO.Internal as Id exposing (Id)



--import Type.Database.User exposing (..)
--import Type.Database.User as User


type alias Table a =
    Dict String (Timestamp a)


type alias Row a =
    ( Id a String, Timestamp a )


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
    { answers : TableView AnswerView
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
    , test_subjects : TableView TestSubjectView
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
    { question : Id Question String
    , test_subject : Id TestSubject String
    , value : String
    , event : Id Event String
    }


type alias AnswerView =
    { question : Question
    , test_subject : TestSubject
    , value : String
    , event : Event
    }


answer : IO Answer Database AnswerView msg
answer =
    entity Answer AnswerView
        |> reference "question" string .question .questions Dict.get .value
        |> reference "test_subject" string .test_subject .test_subjects Dict.get .value
        |> attribute "value" string .value
        |> reference "event" string .event .events Dict.get .value


type alias Coder =
    { user : Id User String
    }


type alias CoderView =
    { user : User
    }


coder : IO Coder Database CoderView msg
coder =
    entity Coder CoderView
        |> reference "user" string .user .users Dict.get .value


type alias Coding =
    { coder : Id Coder String
    }


type alias CodingView =
    { coder : Coder
    }


coding : IO Coding Database CodingView msg
coding =
    entity Coding CodingView
        |> reference "coder" string .coder .coders Dict.get .value


type alias CodingAnswer =
    { coding_question : Id CodingQuestion String

    --, coding_frame : String
    , answer : Id Answer String
    , value : String
    }


type alias CodingAnswerView =
    { coding_question : CodingQuestion

    --, coding_frame : CodingFrame
    , answer : Answer
    , value : String
    }


coding_answer : IO CodingAnswer Database CodingAnswerView msg
coding_answer =
    entity CodingAnswer CodingAnswerView
        |> reference "coding_question" string .coding_question .coding_questions Dict.get .value
        |> reference "answer" string .answer .answers Dict.get .value
        --|> reference "coding_frame" string .coding_frame .coding_frames Dict.get .value
        |> attribute "value" string .value


type alias CodingFrame =
    { answer : Id Answer String
    , coding : Id Coding String
    }


type alias CodingFrameView =
    { answer : Answer
    , coding : Coding
    }


coding_frame : IO CodingFrame Database CodingFrameView msg
coding_frame =
    entity CodingFrame CodingFrameView
        |> reference "answer" string .answer .answers Dict.get .value
        |> reference "coding" string .coding .codings Dict.get .value


type alias CodingQuestion =
    { coding_questionary : Id CodingQuestionary String
    , text : String
    , input_type : Id IT.InputType String
    }


type alias CodingQuestionView =
    { coding_questionary : CodingQuestionary
    , text : String
    , input_type : IT.InputType
    }


coding_question : IO CodingQuestion Database CodingQuestionView msg
coding_question =
    entity CodingQuestion CodingQuestionView
        |> reference "coding_questionary" string .coding_questionary .coding_questionnaries Dict.get .value
        |> attribute "text" string .text
        |> reference "input_type" string .input_type .input_types Dict.get .value
        |> updateEmpty (\x -> { x | text = "Unnamed Coding Question" })


type alias CodingQuestionary =
    { question : Id Question String
    , enabled : Bool
    }


type alias CodingQuestionaryView =
    { question : Question
    , enabled : Bool
    }


coding_questionary : IO CodingQuestionary Database CodingQuestionaryView msg
coding_questionary =
    entity CodingQuestionary CodingQuestionaryView
        |> reference "question" string .question .questions Dict.get .value
        |> attribute "enabled" bool .enabled


type alias Event =
    { place : Id Place String
    , date : String
    , study : Id Study String
    , name : String
    }


type alias EventView =
    { place : Place
    , date : String
    , study : Study
    , name : String
    }


event : IO Event Database EventView msg
event =
    entity Event EventView
        |> reference "place" string .place .places Dict.get .value
        |> attribute "date" string .date
        |> reference "study" string .study .studies Dict.get .value
        |> attribute "name" string .name
        |> updateEmpty (\x -> { x | name = "Unnamed Event" })


type alias Place =
    { name : String
    , latitude : Float
    , longitude : Float
    }


place : IO Place Database Place msg
place =
    entity Place Place
        |> attribute "name" string .name
        |> attribute "latitude" float .latitude
        |> attribute "longitude" float .longitude
        |> updateEmpty (\x -> { x | name = "Unnamed Place" })


type alias Question =
    { questionary : Id Questionary String
    , index : Int
    , text : String
    , input_type : Id IT.InputType String
    }


type alias QuestionView =
    { questionary : Questionary
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
        |> reference "input_type" string .input_type .input_types Dict.get .value
        |> updateEmpty (\x -> { x | text = "Unnamed Question" })


type alias Questionary =
    { name : String
    , study : Id Study String
    }


type alias QuestionaryView =
    { name : String
    , study : Study
    }


questionary : IO Questionary Database QuestionaryView msg
questionary =
    entity Questionary QuestionaryView
        |> attribute "name" string .name
        |> reference "study" string .study .studies Dict.get .value
        |> updateEmpty (\x -> { x | name = "Unnamed Questionary " })


type alias Study =
    { name : String
    , description : String
    , leader : Id User String
    }


type alias StudyView =
    { name : String
    , description : String
    , leader : User
    }


study : IO Study Database StudyView msg
study =
    entity Study StudyView
        |> attribute "name" string .name
        |> attribute "description" string .description
        |> reference "leader" string .leader .users Dict.get .value
        |> updateEmpty (\x -> { x | name = "Unnamed Study " })


type alias TestSubject =
    { id : String
    , event : Id Event String
    , infos : Dict String String
    }


type alias TestSubjectView =
    { id : String
    , event : Event
    , infos : Dict String String
    }


test_subject : IO TestSubject Database TestSubjectView msg
test_subject =
    entity TestSubject TestSubjectView
        |> attribute "id" string .id
        |> reference "event" string .event .events Dict.get .value
        |> attribute "infos" (dict string string) .infos


type alias User =
    { name : Maybe String
    , email : Maybe String
    , last_login : Int
    }


user : IO User Database User msg
user =
    entity User User
        |> attribute "name" (maybe string) .name
        |> attribute "email" (maybe string) .email
        |> attribute "last_login" int .last_login


type alias Timestamp a =
    { creator : Id User String
    , created : Int
    , modified : Int
    , accessed : Int
    , deleted : Maybe Int
    , value : a
    }


type alias TimestampView a =
    { creator : User
    , created : Int
    , modified : Int
    , accessed : Int
    , deleted : Maybe Int
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
                |> attribute "deleted" (maybe int) .deleted
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


type Type
    = AnswerType
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
    | TestSubjectType
    | InputTypeType InputTypeKind


type InputTypeKind
    = ShortKind
    | LongKind
    | ListKind


updateEmpty : (a -> a) -> IO a b c msg -> IO a b c msg
updateEmpty f prev =
    { prev | empty = f prev.empty }


rows : Table a -> List (Row a)
rows old =
    Dict.toList old
        |> List.map (Tuple.mapFirst Id.box)
