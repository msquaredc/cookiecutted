module AquisitionTest exposing (..)

import Expect
import Dict exposing (Dict)
import Test exposing (..)
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
    ]