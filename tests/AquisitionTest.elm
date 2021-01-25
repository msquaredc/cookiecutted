module AquisitionTest exposing (..)

import Expect
import Dict exposing (Dict)
import Test exposing (..)
import Fuzz
import Type.Database exposing (..)
import Type.IO exposing (..)
import Type.Database.TypeMatching as Match
import Type.Database.Aquisition as Aq exposing (..)
import Type.IO.Internal exposing (Id, unbox, box)


aquireQuestion : Id Answer String -> Database -> String
aquireQuestion id db =
    Aquisition identity id
    -- |> start (Value .question) db.questions (Value .text)
    |> moveReferenceSingle (Raw Tuple.first) db.answers (Value .question)
    |> add (Raw Tuple.first) db.questions (Value .text)
    |> aquire
    |> List.head
    |> Maybe.withDefault ""

suite : Test
suite = 
    describe "Main" [
        fuzz string.fuzzer "string" <|
            \val ->
                Expect.equal val val
        , fuzz3 database.fuzzer (timestamp question).fuzzer (timestamp answer).fuzzer "Pathlength 2" <|
            \db q a-> 
                {db | questions = Dict.insert (unbox a.value.question) q db.questions}
                |> \x -> {x | answers = Dict.insert "myspecialkey" a x.answers}
                |> aquireQuestion (box "myspecialkey")
                |> Expect.equal q.value.text
        , skip <| fuzz (Fuzz.tuple (
            (Fuzz.tuple3 (database.fuzzer, Fuzz.tuple (Fuzz.string, (timestamp study).fuzzer), Fuzz.tuple (Fuzz.string, (timestamp event).fuzzer))),
            (Fuzz.list <| 
                Fuzz.tuple3 
                    (Fuzz.string, 
                    (timestamp questionary).fuzzer, 
                    Fuzz.list <| 
                        Fuzz.tuple3
                            (Fuzz.string,
                            Fuzz.tuple ((timestamp question).fuzzer, Fuzz.list (Fuzz.tuple (Fuzz.string, (timestamp answer).fuzzer))),
                            Fuzz.list <| 
                                Fuzz.tuple3
                                    (Fuzz.string,
                                    (timestamp coding_questionary).fuzzer,
                                    Fuzz.list <|
                                        Fuzz.tuple
                                        (Fuzz.string,
                                        (timestamp coding_question).fuzzer)
                                    )
                            )
                    )
            ))) "Integration Test" <|
        \((db, (mystudyid, mystudy), (myeventid, myevent)),qlist) ->
            let
                myeventvalue = myevent.value
                myneweventvalue = {myeventvalue | study = box mystudyid}
                mynewevent = {myevent | value = myneweventvalue}
            in
            {db | studies = Dict.insert mystudyid mystudy db.studies}
            |> \x -> {x | events = Dict.insert myeventid mynewevent x.events}
            |> \x2 -> List.foldl (insertQuestionary (box mystudyid) (box myeventid)) x2 qlist
            |> Expect.equal db
    ]

insertQuestionary : Id Study String 
    -> Id Event String 
    -> (String, Timestamp Questionary, List (String, (Timestamp Question, List (String, Timestamp Answer)), List (String, Timestamp CodingQuestionary, a))) 
    -> Database -> Database
insertQuestionary studyid eventid (myquestionaryid, myquestionary, qlist) db =
    let
        myquestionaryvalue = myquestionary.value
        mynewquestionaryvalue = {myquestionaryvalue | study = studyid}
        mynewquestionary = {myquestionary | value = mynewquestionaryvalue}
    in
        {db | questionnaries = Dict.insert myquestionaryid mynewquestionary db.questionnaries}
        |> \x -> List.foldl (insertQuestion (box myquestionaryid) eventid) x qlist

insertQuestion : Id Questionary String 
    -> Id Event String 
    -> (String, (Timestamp Question, List (String, Timestamp Answer)), List (String, Timestamp CodingQuestionary, a)) 
    -> Database -> Database
insertQuestion questionaryid eventid (myquestionid, (myquestion, myanswers), qlist) db =
    let
        myquestionvalue = myquestion.value
        mynewquestionvalue = {myquestionvalue | questionary = questionaryid}
        mynewquestion = {myquestion | value = mynewquestionvalue}
    in
        {db | questions = Dict.insert myquestionid mynewquestion db.questions}
        |> \x -> List.foldl (insertAnswer (box myquestionid) eventid) x myanswers
        |> \x2 -> List.foldl (insertCodingQuestionary (box myquestionid)) x2 qlist

insertAnswer : Id Question String -> Id Event String -> (String, Timestamp Answer) -> Database -> Database
insertAnswer questionid eventid (myanswerid, myanswer) db =
    let
        myanswervalue = myanswer.value
        mynewanswervalue = {myanswervalue | question = questionid
                                          , event = eventid}
        mynewanswer = {myanswer | value = mynewanswervalue}
    in
        {db | answers = Dict.insert myanswerid mynewanswer db.answers}

insertCodingQuestionary : Id Question String -> (String, Timestamp CodingQuestionary, a) -> Database -> Database
insertCodingQuestionary questionid (mycoding_questionaryid, mycoding_questionary, qlist) db = 
    let
        mycoding_questionaryvalue = mycoding_questionary.value
        mynewcoding_questionaryvalue = {mycoding_questionaryvalue | question = questionid}
        mynewcoding_questionary = {mycoding_questionary | value = mynewcoding_questionaryvalue}
    in
        {db | coding_questionnaries = Dict.insert mycoding_questionaryid mynewcoding_questionary db.coding_questionnaries}
        --|> \x -> List.foldl (insertQuestion (box myquestionaryid) eventid) x qlist
