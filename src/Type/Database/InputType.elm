module Type.Database.InputType exposing (InputType(..), ListConfig, LongAnswerConfig, ShortAnswerConfig, SingleInputType(..), input_type, listConfig, longAnswerConfig, toString)

import Fuzz
import Json.Decode
import Json.Encode
import Type.IO exposing (IO, attribute, entity, int, list, maybe, string, substruct)
import Type.IO.Encoder as Encoder exposing (Encoder(..))
import Type.IO.Form as Form exposing (Form)
import Type.IO.Setter as Updater exposing (Updater)
import Type.IO.ToString exposing (ToString)


type InputType
    = ShortAnswer ShortAnswerConfig
    | LongAnswer LongAnswerConfig
    | List ListConfig



-- | DropDown (List String)
-- | LinearScale (Dict Int String)
-- | Matrix SingleInputType (List String) (List String)


type SingleInputType
    = Radio
    | Box


type alias ShortAnswerConfig =
    { label : Maybe String
    , placeholder : Maybe String
    , minLength : Maybe Int
    , maxLength : Maybe Int
    , pattern : Maybe String
    }


shortAnswerConfig : IO ShortAnswerConfig db ShortAnswerConfig msg
shortAnswerConfig =
    entity ShortAnswerConfig ShortAnswerConfig
        |> attribute "label" (maybe string) .label
        |> attribute "placeholder" (maybe string) .placeholder
        |> attribute "minLength" (maybe int) .minLength
        |> attribute "maxLength" (maybe int) .maxLength
        |> attribute "pattern" (maybe string) .pattern


type alias LongAnswerConfig =
    { label : Maybe String
    , placeholder : Maybe String
    , minLength : Maybe Int
    , maxLength : Maybe Int
    , pattern : Maybe String
    , rows : Maybe Int
    , cols : Maybe Int
    }


longAnswerConfig : IO LongAnswerConfig db LongAnswerConfig msg
longAnswerConfig =
    entity LongAnswerConfig LongAnswerConfig
        |> attribute "label" (maybe string) .label
        |> attribute "placeholder" (maybe string) .placeholder
        |> attribute "minLength" (maybe int) .minLength
        |> attribute "maxLength" (maybe int) .maxLength
        |> attribute "pattern" (maybe string) .pattern
        |> attribute "rows" (maybe int) .rows
        |> attribute "cols" (maybe int) .cols


singleInputType : IO SingleInputType db SingleInputType msg
singleInputType =
    { decoder = singleInputTypeDecoder
    , strDecoder = \_ -> singleInputTypeDecoder
    , encoder = singleInputTypeEncoder
    , fuzzer = Fuzz.oneOf [ Fuzz.constant Box, Fuzz.constant Radio ]
    , toString = singleInputTypeToString
    , viewer = \_ full -> Just full
    , empty = Box
    , fields = []
    , form = singleInputTypeForm
    , updater = singleInputTypeUpdater
    }


singleInputTypeDecoder : Json.Decode.Decoder SingleInputType
singleInputTypeDecoder =
    let
        helper : String -> Json.Decode.Decoder SingleInputType
        helper name =
            case name of
                "radio" ->
                    Json.Decode.succeed Radio

                "box" ->
                    Json.Decode.succeed Box

                _ ->
                    Json.Decode.fail <| "I need either radio or box, but i got" ++ name
    in
    Json.Decode.string
        |> Json.Decode.andThen helper


singleInputTypeEncoder : Encoder SingleInputType
singleInputTypeEncoder =
    SingleEncoder <|
        \value ->
            case value of
                Box ->
                    Json.Encode.string "box"

                Radio ->
                    Json.Encode.string "radio"


singleInputTypeToString : ToString SingleInputType
singleInputTypeToString name value =
    case value of
        Box ->
            Ok "box"

        Radio ->
            Ok "radio"


singleInputTypeForm : Form SingleInputType msg
singleInputTypeForm name callback kind label f =
    case kind of
        Box ->
            Ok <| f "box" (callback << Form.StringMsg << Just)

        Radio ->
            Ok <| f "radio" (callback << Form.StringMsg << Just)


singleInputTypeUpdater : Updater SingleInputType
singleInputTypeUpdater msg val =
    case msg of
        Updater.StringMsg "box" ->
            Ok Box

        Updater.StringMsg "radio" ->
            Ok Radio

        _ ->
            Err Updater.InvalidValue


type alias ListConfig =
    { singleInput : SingleInputType
    , choices : List String
    }


listConfig : IO ListConfig db ListConfig msg
listConfig =
    entity ListConfig ListConfig
        |> substruct "singleInput" singleInputType .singleInput
        |> attribute "choices" (list string) .choices
        |> updateEmpty (\x -> { x | choices = [ "Unnamed Choice" ] })


toString : InputType -> String
toString kind =
    case kind of
        ShortAnswer _ ->
            "Short Answer"

        LongAnswer _ ->
            "Long Answer"

        List { singleInput } ->
            case singleInput of
                Radio ->
                    "Multiple Choice"

                Box ->
                    "Boxes"



-- DropDown _ ->
--     "DropDown Menu"
-- LinearScale _ ->
--     "Linear Scale"
-- Matrix Radio _ _ ->
--     "Grid of Multiple Choices"
-- Matrix Box _ _ ->
--     "Grid of Boxes"
-- decodeInputType : Json.Decoder.Decoder InputType
-- decodeInputType =


input_type : IO InputType db InputType msg
input_type =
    { decoder = inputTypeDecoder
    , strDecoder = \_ -> inputTypeDecoder
    , encoder = inputTypeEncoder
    , fuzzer = inputTypeFuzzer
    , toString = inputTypeToString
    , viewer = \_ full -> Just full
    , empty = ShortAnswer shortAnswerConfig.empty
    , fields = []
    , form = inputTypeForm
    , updater = inputTypeUpdater
    }


inputTypeDecoder : Json.Decode.Decoder InputType
inputTypeDecoder =
    Json.Decode.oneOf
        [ Json.Decode.map LongAnswer longAnswerConfig.decoder
        , Json.Decode.map ShortAnswer shortAnswerConfig.decoder
        , Json.Decode.map List listConfig.decoder
        ]


inputTypeEncoder : Encoder InputType
inputTypeEncoder =
    SingleEncoder <|
        \value ->
            case value of
                ShortAnswer v ->
                    Encoder.collapseEncoder shortAnswerConfig.encoder v

                LongAnswer v ->
                    Encoder.collapseEncoder longAnswerConfig.encoder v

                List v ->
                    Encoder.collapseEncoder listConfig.encoder v


inputTypeFuzzer : Fuzz.Fuzzer InputType
inputTypeFuzzer =
    Fuzz.oneOf
        [ Fuzz.map ShortAnswer shortAnswerConfig.fuzzer
        , Fuzz.map LongAnswer longAnswerConfig.fuzzer
        , Fuzz.map List listConfig.fuzzer
        ]


inputTypeToString : ToString InputType
inputTypeToString name value =
    case value of
        ShortAnswer v ->
            shortAnswerConfig.toString name v

        LongAnswer v ->
            longAnswerConfig.toString name v

        List v ->
            listConfig.toString name v


inputTypeForm : Form InputType msg
inputTypeForm name callback kind =
    case kind of
        ShortAnswer v ->
            shortAnswerConfig.form name callback v

        LongAnswer v ->
            longAnswerConfig.form name callback v

        List v ->
            listConfig.form name callback v


inputTypeUpdater : Updater InputType
inputTypeUpdater msg val =
    case msg of
        Updater.Custom kind mbMsg ->
            case kind of
                "Short Answer" ->
                    case ( val, mbMsg ) of
                        ( ShortAnswer v, Just msg_ ) ->
                            Result.map ShortAnswer <| shortAnswerConfig.updater msg_ v

                        ( ShortAnswer _, Nothing ) ->
                            Ok val

                        ( _, Just msg_ ) ->
                            Result.map ShortAnswer <| shortAnswerConfig.updater msg_ shortAnswerConfig.empty

                        ( _, Nothing ) ->
                            Ok <| ShortAnswer shortAnswerConfig.empty

                "Long Answer" ->
                    case ( val, mbMsg ) of
                        ( LongAnswer v, Just msg_ ) ->
                            Result.map LongAnswer <| longAnswerConfig.updater msg_ v

                        ( LongAnswer _, Nothing ) ->
                            Ok val

                        ( _, Just msg_ ) ->
                            Result.map LongAnswer <| longAnswerConfig.updater msg_ longAnswerConfig.empty

                        ( _, Nothing ) ->
                            Ok <| LongAnswer longAnswerConfig.empty

                s ->
                    Err <| Updater.CustomError <| "Incorrect string: " ++ s

        {- case (kind, val, mbMsg) of
           ("Multiple Choice", List v, Just msg_) ->
               Result.map List <| listConfig.updater msg_ {v | singleInput = Radio}
           ("Multiple Choice", List v, Nothing) ->
               Ok <| List {v | singleInput = Radio}
           ("Multiple Choice", _, Just msg_) ->
               Result.map List <| listConfig.updater msg_ <| ListConfig Radio ["Unnamed Choice"]
           ("Multiple Choice", _, Nothing) ->
               Ok <| List <| ListConfig Radio ["Unnamed Choice"]
           ("Boxes", List v, Just msg_) ->
               Result.map List <| listConfig.updater msg_ {v | singleInput = Box}
           ("Boxes", List v, Nothing) ->
               Ok <| List <| {v |singleInput = Box}
           ("Boxes", _, Just msg_) ->
               Result.map List <| listConfig.updater msg_ <| ListConfig Box ["Unnamed Choice"]
           ("Boxes", _, Nothing) ->
               Ok <| List <| ListConfig Box ["Unnamed Choice"]
           _ ->
               Err Updater.InvalidValue
        -}
        Updater.AttributeMsg _ _ ->
            case val of
                ShortAnswer v ->
                    Result.map ShortAnswer <| shortAnswerConfig.updater msg v

                LongAnswer v ->
                    Result.map LongAnswer <| longAnswerConfig.updater msg v

                List v ->
                    Result.map List <| listConfig.updater msg v

        _ ->
            Err <| Updater.CustomError <| "Tried to update an InputType without using Custom"


updateEmpty : (a -> a) -> IO a b c msg -> IO a b c msg
updateEmpty f prev =
    { prev | empty = f prev.empty }
