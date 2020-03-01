module Type.Database.InputType exposing (..)

import Dict exposing (Dict)
import Fuzz
import Json.Decode
import Json.Encode
import Type.IO exposing (..)
import Type.IO.Encoder exposing (Encoder(..))
import Type.IO.Form as Form


type InputType
    = ShortAnswer
    | LongAnswer
    | List SingleInputType (List String)
    | DropDown (List String)
    | LinearScale (Dict Int String)
    | Matrix SingleInputType (List String) (List String)


type SingleInputType
    = Radio
    | Box


type alias ShortAnswerConfig =
    { label : Maybe String
    , placeholder : Maybe String
    , minLength : Maybe Int
    , maxLength : Maybe Int
    , pattern : Maybe String
    , type_ : String
    }


shortAnswerConfig : IO ShortAnswerConfig db ShortAnswerConfig msg
shortAnswerConfig =
    entity ShortAnswerConfig ShortAnswerConfig
        |> attribute "label" (maybe string) .label
        |> attribute "placeholder" (maybe string) .placeholder
        |> attribute "minLength" (maybe int) .minLength
        |> attribute "maxLength" (maybe int) .maxLength
        |> attribute "pattern" (maybe string) .pattern
        |> attribute "type_" string .type_
        |> updateEmpty (\x -> { x | type_ = "text" })


inputTypes : List InputType
inputTypes =
    [ ShortAnswer
    , LongAnswer
    , List Radio []
    , List Box []
    , DropDown []
    , LinearScale Dict.empty
    , Matrix Radio [] []
    , Matrix Box [] []
    ]


toString : InputType -> String
toString kind =
    case kind of
        ShortAnswer ->
            "Short Answer"

        LongAnswer ->
            "Long Answer"

        List Radio _ ->
            "Multiple Choice"

        List Box _ ->
            "Boxes"

        DropDown _ ->
            "DropDown Menu"

        LinearScale _ ->
            "Linear Scale"

        Matrix Radio _ _ ->
            "Grid of Multiple Choices"

        Matrix Box _ _ ->
            "Grid of Boxes"


fromString : String -> Maybe InputType
fromString name =
    case name of
        "Short Answer" ->
            Just ShortAnswer

        "Long Answer" ->
            Just LongAnswer

        "Multiple Choice" ->
            Just <| List Radio []

        "Boxes" ->
            Just <| List Box []

        "DropDown Menu" ->
            Just <| DropDown []

        "Linear Scale" ->
            Just <| LinearScale Dict.empty

        "Grid of Multiple Choices" ->
            Just <| Matrix Radio [] []

        "Grid of Boxes" ->
            Just <| Matrix Box [] []

        _ ->
            Nothing



-- decodeInputType : Json.Decoder.Decoder InputType
-- decodeInputType =


input_type : IO InputType db InputType msg
input_type =
    { decoder = Json.Decode.succeed ShortAnswer
    , strDecoder = \_ -> Json.Decode.succeed ShortAnswer
    , encoder = SingleEncoder (\_ -> Json.Encode.string "bool")
    , fuzzer = Fuzz.constant ShortAnswer
    , toString = \_ a -> Ok (toString a)
    , viewer = \_ full -> Just full
    , empty = ShortAnswer
    , fields = []
    , form = \_ _ _ _ _ -> Err Form.KeyError
    , updater = \_ a -> Ok a
    }


updateEmpty : (a -> a) -> IO a b c msg -> IO a b c msg
updateEmpty f prev =
    { prev | empty = f prev.empty }
