module Example exposing (..)

import Dict
import Expect
import Fuzz
import Json.Decode
import Json.Encode
import Test exposing (..)
import Type.Database exposing (..)
import Type.IO exposing (..)
import Type.IO.Setter as Set exposing (Msg(..))
import Type.IO.Internal exposing (box)


suite : Test
suite =
    describe "Main Test Suite"
        [ describe "IO Encode/Decode Tests"
            [ describe "String Tests"
                [ fuzz string.fuzzer "string" <|
                    \val ->
                        encode string.encoder val
                            |> Json.Decode.decodeValue string.decoder
                            |> Expect.equal (Ok val)
                , fuzz (array string).fuzzer "array string" <|
                    \val ->
                        encode (array string).encoder val
                            |> Json.Decode.decodeValue (array string).decoder
                            |> Expect.equal (Ok val)
                , fuzz (dict string (maybe float)).fuzzer "dict string maybe float" <|
                    \val ->
                        Json.Encode.encode 0 (encode (dict string (maybe float)).encoder val)
                            |> Json.Decode.decodeString (dict string (maybe float)).decoder
                            |> Expect.equal (Ok val)
                , fuzz2 string.fuzzer (maybe float).fuzzer "object" <|
                    \key v ->
                        Json.Encode.object [ ( key, encode (maybe float).encoder v ) ]
                            |> Json.Decode.decodeValue (Json.Decode.dict (maybe float).decoder)
                            |> Expect.equal (Ok (Dict.singleton key v))
                , fuzz2 string.fuzzer string.fuzzer "string encoder" <|
                    \key value ->
                        Json.Encode.object [ ( key, encode string.encoder value ) ]
                            |> Expect.equal (encode (dict string string).encoder <| Dict.fromList [ ( key, value ) ])
                , fuzz2 string.fuzzer Fuzz.string "dict string string" <|
                    \key value ->
                        Expect.equal
                            (Ok <| Dict.singleton key value)
                            (Json.Decode.decodeValue
                                (Json.Decode.dict Json.Decode.string)
                                (Json.Encode.object [ ( key, Json.Encode.string value ) ])
                            )
                ]
            , describe "Int Tests"
                [ fuzz int.fuzzer "int" <|
                    \val ->
                        encode int.encoder val
                            |> Json.Decode.decodeValue int.decoder
                            |> Expect.equal (Ok val)
                , fuzz (maybe int).fuzzer "maybe int" <|
                    \val ->
                        encode (maybe int).encoder val
                            |> Json.Decode.decodeValue (maybe int).decoder
                            |> Expect.equal (Ok val)
                , fuzz (dict int (maybe float)).fuzzer "dict int maybe float" <|
                    \val ->
                        encode (dict int (maybe float)).encoder val
                            |> Json.Decode.decodeValue (dict int (maybe float)).decoder
                            |> Expect.equal (Ok val)
                , fuzz (array int).fuzzer "array int" <|
                    \val ->
                        encode (array int).encoder val
                            |> Json.Decode.decodeValue (array int).decoder
                            |> Expect.equal (Ok val)
                ]
            , fuzz float.fuzzer "float" <|
                \val ->
                    encode float.encoder val
                        |> Json.Decode.decodeValue float.decoder
                        |> Expect.equal (Ok val)
            , fuzz bool.fuzzer "bool" <|
                \val ->
                    encode bool.encoder val
                        |> Json.Decode.decodeValue bool.decoder
                        |> Expect.equal (Ok val)
            , fuzz (list float).fuzzer "list float" <|
                \val ->
                    encode (list float).encoder val
                        |> Json.Decode.decodeValue (list float).decoder
                        |> Expect.equal (Ok val)
            ]
        , describe "Database Tests"
            [ fuzz answer.fuzzer "Answer" <|
                \val ->
                    encode answer.encoder val
                        |> Json.Decode.decodeValue answer.decoder
                        |> Expect.equal (Ok val)
            , fuzz coder.fuzzer "Coder" <|
                \val ->
                    encode coder.encoder val
                        |> Json.Decode.decodeValue coder.decoder
                        |> Expect.equal (Ok val)
            , fuzz coding.fuzzer "Coding" <|
                \val ->
                    encode coding.encoder val
                        |> Json.Decode.decodeValue coding.decoder
                        |> Expect.equal (Ok val)
            , fuzz coding_answer.fuzzer "Coding Answer" <|
                \val ->
                    encode coding_answer.encoder val
                        |> Json.Decode.decodeValue coding_answer.decoder
                        |> Expect.equal (Ok val)
            , fuzz coding_frame.fuzzer "Coding Frame" <|
                \val ->
                    encode coding_frame.encoder val
                        |> Json.Decode.decodeValue coding_frame.decoder
                        |> Expect.equal (Ok val)
            , fuzz question.fuzzer "Question" <|
                \val ->
                    encode question.encoder val
                        |> Json.Decode.decodeValue question.decoder
                        |> Expect.equal (Ok val)
            ]
        , 
            describe "FullTest"
                [ fuzz database.fuzzer "Database" <|
                    \db ->
                        encode database.encoder db
                            |> Json.Decode.decodeValue database.decoder
                            |> Expect.equal (Ok db)
                ]
        , describe "SetterTest"
            [ fuzz2 answer.fuzzer string.fuzzer "Answer: Question" <|
                \answ question ->
                    Expect.equal
                        (Ok { answ | question = box question })
                        (answer.updater (AttributeMsg "question" <| StringMsg question) answ)
            , fuzz2 answer.fuzzer string.fuzzer "Answer: User" <|
                \answ test_subject ->
                    Expect.equal
                        (Ok { answ | test_subject = box test_subject })
                        (answer.updater (AttributeMsg "test_subject" <| StringMsg test_subject) answ)
            , fuzz2 answer.fuzzer string.fuzzer "Answer: Value" <|
                \answ question ->
                    Expect.equal
                        (Ok { answ | question = box question })
                        (answer.updater (AttributeMsg "question" <| StringMsg question) answ)
            , fuzz2 coder.fuzzer string.fuzzer "Coder: User" <|
                \cod user ->
                    Expect.equal
                        (Ok { cod | user = box user })
                        (coder.updater (AttributeMsg "user" <| StringMsg user) cod)
            , fuzz (table answer).fuzzer string.fuzzer "Pathlength 2" <|
                \a q ->
                    case List.head <| Dict.keys a of
                        Just id ->
                            case Dict.get id a of
                                Just tablerow ->
                                    let
                                        oldanswer =
                                            tablerow.value

                                        newanswer =
                                            { oldanswer | question = box q }

                                        newtablerow =
                                            { tablerow | value = newanswer }

                                        res =
                                            Dict.insert id newtablerow a

                                        f id2 =
                                            Set.DictKeyMsg id2 <|
                                                Set.AttributeMsg "value" <|
                                                    Set.AttributeMsg "question" <|
                                                        Set.StringMsg q
                                    in
                                    Expect.equal
                                        (Ok res)
                                        ((table answer).updater (f id) a)

                                Nothing ->
                                    Expect.true "blupp" True

                        Nothing ->
                            Expect.true "bla" True
            ]
        ]
