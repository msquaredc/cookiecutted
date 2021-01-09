module Type.IO exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Fuzz exposing (Fuzzer)
import Json.Decode
import Json.Encode
import Type.IO.Decoder as Decoder exposing (Decoder)
import Type.IO.Encoder as Encoder exposing (Encoder)
import Type.IO.Form as Form exposing (Form)
import Type.IO.ToString as ToString exposing (ToString)
import Type.IO.Setter as Update exposing (PartialUpdater)
import Type.IO.Internal as Id exposing (Id)
import Type.IO.Viewer as Viewer exposing (Viewer)


type alias PartialIO delta full db view msg =
    { decoder : Decoder delta
    , strDecoder : String -> Decoder delta
    , toString : ToString full
    , encoder : Encoder full
    , fuzzer : Fuzzer delta
    , viewer : Viewer db full view
    , empty : delta
    , fields : List String
    , form : Form full msg
    , updater : PartialUpdater full delta
    }

type alias IO kind db view msg =
    PartialIO kind kind db view msg


type alias DatatypeIO kind db msg =
    IO kind db kind msg


type Reference kind
    = Reference String


encode : Encoder a -> a -> Json.Encode.Value
encode =
    Encoder.collapseEncoder


int : DatatypeIO Int db msg
int =
    { decoder = Decoder.int
    , strDecoder = \_ -> Decoder.int
    , toString = ToString.int
    , encoder = Encoder.int
    , fuzzer = Fuzz.int
    , viewer = Viewer.int
    , empty = 0
    , fields = []
    , form = Form.int
    , updater = Update.int
    }


string : DatatypeIO String db msg
string =
    { decoder = Decoder.string
    , strDecoder = Json.Decode.succeed
    , toString = ToString.string
    , encoder = Encoder.string
    , fuzzer = Fuzz.string
    , viewer = Viewer.string
    , empty = ""
    , fields = []
    , form = Form.string
    , updater = Update.string
    }


float : DatatypeIO Float db msg
float =
    { decoder = Decoder.float
    , strDecoder = \_ -> Decoder.float
    , toString = ToString.float
    , encoder = Encoder.float
    , fuzzer = Fuzz.float
    , viewer = Viewer.float
    , empty = 0
    , fields = []
    , form = Form.float
    , updater = Update.float
    }


bool : DatatypeIO Bool db msg
bool =
    { decoder = Decoder.bool
    , strDecoder = \_ -> Decoder.bool
    , toString = ToString.bool
    , encoder = Encoder.bool
    , fuzzer = Fuzz.bool
    , viewer = Viewer.bool
    , empty = False
    , fields = []
    , form = Form.bool
    , updater = Update.bool
    }


maybe : IO b db d msg -> IO (Maybe b) db (Maybe d) msg
maybe old =
    { decoder = Decoder.maybe old.decoder
    , strDecoder = \a -> Decoder.maybe (old.strDecoder a)
    , toString = ToString.maybe old.toString
    , encoder = Encoder.maybe old.encoder
    , fuzzer = Fuzz.maybe old.fuzzer
    , viewer = Viewer.maybe old.viewer
    , empty = Nothing
    , fields = []
    , form = Form.maybe old.form
    , updater = Update.maybe old.empty old.updater
    }


list : DatatypeIO b db msg -> DatatypeIO (List b) db msg
list old =
    { decoder = Decoder.list old.decoder
    , strDecoder = \a -> Decoder.list (old.strDecoder a)
    , toString = ToString.list old.toString
    , encoder = Encoder.list old.encoder
    , fuzzer = Fuzz.list old.fuzzer
    , viewer = Viewer.list old.viewer
    , empty = []
    , fields = []
    , form = Form.list old.form
    , updater = Update.list old.empty old.updater
    }


dict : IO comparableA db comparableB msg -> IO b db c msg -> IO (Dict comparableA b) db (Dict comparableB c) msg
dict keys values =
    { decoder = Decoder.dict keys.strDecoder values.decoder
    , strDecoder = \a -> Decoder.dict keys.strDecoder (values.strDecoder a)
    , toString = ToString.dict keys.toString values.toString
    , encoder = Encoder.dict (\x -> Result.toMaybe (keys.toString "" x)) values.encoder
    , fuzzer =
        ( keys.fuzzer, values.fuzzer )
            |> Fuzz.tuple
            |> Fuzz.list
            |> Fuzz.map Dict.fromList
    , viewer = Viewer.dict keys.viewer values.viewer
    , empty = Dict.empty
    , fields = []
    , form = Form.dict (\x -> Result.toMaybe (keys.toString "" x)) values.form
    , updater = Update.dict keys.empty values.empty (\x -> Result.toMaybe (keys.toString "" x)) keys.updater values.updater
    }


result : DatatypeIO error db msg -> DatatypeIO a db msg -> DatatypeIO (Result error a) db msg
result err val =
    { decoder = Decoder.result err.decoder val.decoder
    , strDecoder = \a -> Decoder.result (err.strDecoder a) (val.strDecoder a)
    , toString = ToString.result err.toString val.toString
    , fuzzer = Fuzz.result err.fuzzer val.fuzzer
    , encoder = Encoder.result err.encoder val.encoder
    , viewer = Viewer.result err.viewer val.viewer
    , empty = Result.Err err.empty
    , fields = []
    , form = Form.result err.form val.form
    , updater = Update.result err.empty val.empty err.updater val.updater
    }


array : DatatypeIO a db msg -> DatatypeIO (Array a) db msg
array old =
    { decoder = Decoder.array old.decoder
    , strDecoder = \a -> Decoder.array (old.strDecoder a)
    , encoder = Encoder.array old.encoder
    , fuzzer = Fuzz.array old.fuzzer
    , toString = ToString.array old.toString
    , viewer = Viewer.array old.viewer
    , empty = Array.empty
    , fields = []
    , form = Form.array old.form
    , updater = Update.array old.empty old.updater
    }


entity : b -> c -> PartialIO b a db c msg
entity new view =
    { decoder = Decoder.entity new
    , strDecoder = \a -> Decoder.entity new
    , toString = ToString.entity new
    , encoder = Encoder.entity
    , fuzzer = Fuzz.constant new
    , viewer = Viewer.entity view
    , empty = new
    , fields = []
    , form = Form.entity
    , updater = Update.entity new
    }


attribute : String -> IO a db e msg -> (c -> a) -> PartialIO (a -> b) c db (a -> d) msg -> PartialIO b c db d msg
attribute name def getter parent =
    { decoder = Decoder.attribute name def.decoder parent.decoder
    , strDecoder = \a -> Decoder.attribute name (def.strDecoder a) (parent.strDecoder a)
    , toString = ToString.attribute name def.toString getter parent.toString
    , encoder = Encoder.attribute name def.encoder getter parent.encoder
    , fuzzer = Fuzz.andMap def.fuzzer parent.fuzzer
    , viewer = Viewer.attribute getter parent.viewer
    , empty = parent.empty def.empty
    , fields = parent.fields ++ [ name ]
    , form = Form.attribute name getter def.form parent.form
    , updater = Update.attribute name getter def.updater parent.updater
    }


reference :
    String
    -> IO comparable db comparable msg -- Key
    -> (c -> Id g comparable) -- KeyGetter
    -> (db -> f) -- lookup
    -> (comparable -> f -> Maybe d) -- MaybeGetter
    -> (d -> a)
    -> PartialIO ((Id g comparable) -> b) c db (a -> e) msg -- Old Entity
    -> PartialIO b c db e msg
reference name def getter lookup foreigngetter post parent =
    { decoder = Decoder.reference name (Json.Decode.map Id.box def.decoder) parent.decoder
    , strDecoder = \a -> Decoder.reference name (Json.Decode.map Id.box (def.strDecoder a)) (parent.strDecoder a)
    , encoder = Encoder.reference name getter def.encoder parent.encoder 
    , toString = ToString.reference name getter def.toString parent.toString
    , fuzzer = Fuzz.andMap (Fuzz.map Id.box def.fuzzer) parent.fuzzer
    , viewer = Viewer.reference getter lookup foreigngetter post parent.viewer
    , empty = parent.empty (Id.box def.empty)
    , fields = parent.fields ++ [ name ]
    , form = Form.reference name getter def.form parent.form
    , updater = Update.reference name getter def.updater parent.updater
    }


references :
    String
    -> IO comparable db comparable msg
    -> (c -> List comparable)
    -> (db -> f)
    -> (comparable -> f -> Maybe d)
    -> (d -> a)
    -> PartialIO (List comparable -> b) c db (List a -> e) msg
    -> PartialIO b c db e msg
references name def getter lookup foreigngetter post parent =
    { decoder = Decoder.references name def.decoder parent.decoder
    , strDecoder = \a -> Decoder.references name (def.strDecoder a) (parent.strDecoder a)
    , encoder = Encoder.references name getter def.encoder parent.encoder
    , toString = ToString.references name getter def.toString parent.toString
    , fuzzer = Fuzz.andMap (Fuzz.list def.fuzzer) parent.fuzzer 
    , viewer = Viewer.references getter lookup foreigngetter post parent.viewer
    , empty = parent.empty (list def).empty
    , fields = parent.fields ++ [ name ]
    , form = Form.references name getter (list def).form parent.form
    , updater = Update.references name getter def.empty def.updater parent.updater
    }


substruct : String -> IO a db b msg -> (e -> a) -> PartialIO (a -> c) e db (b -> d) msg -> PartialIO c e db d msg
substruct name struct getter old =
    { decoder = Decoder.substruct name struct.decoder old.decoder
    , strDecoder = \a -> Decoder.substruct name (struct.strDecoder a) (old.strDecoder a)
    , fuzzer = Fuzz.andMap struct.fuzzer old.fuzzer
    , toString = ToString.substruct name struct.toString getter old.toString
    , encoder = Encoder.substruct name struct.encoder getter old.encoder
    , empty = old.empty struct.empty
    , viewer = Viewer.substruct getter struct.viewer old.viewer
    , fields = old.fields ++ List.map (\x -> name ++ "." ++ x) struct.fields
    , form = Form.substruct name getter struct.form old.form
    , updater = Update.substruct name getter struct.updater old.updater
    }


reference_fuzzer : Fuzzer (Reference a)
reference_fuzzer =
    Fuzz.string
        |> Fuzz.map (\x -> Reference x)


map_decoder_maybe : (Decoder (delta -> target) -> Decoder target) -> Decoder (Maybe delta -> target) -> Decoder target
map_decoder_maybe olddecoder newhandle =
    Json.Decode.map map_maybe_func newhandle
        |> olddecoder


map_maybe_func : (Maybe delta -> target) -> delta -> target
map_maybe_func func val =
    func (Just val)

form2update : Form.UpdateMsg -> Maybe (Update.Msg)
form2update fmsg =
    case fmsg of
        Form.IntMsg Nothing ->
            Nothing
        Form.IntMsg (Just val) ->
            Just (Update.IntMsg val)
        Form.StringMsg Nothing ->
            Nothing
        Form.StringMsg (Just val) ->
            Just (Update.StringMsg val)
        Form.FloatMsg Nothing ->
            Nothing
        Form.FloatMsg (Just val) ->
            Just (Update.FloatMsg val)
        Form.BoolMsg _ ->
            Just (Update.BoolUpdateMsg not)
        Form.ListMsg index msg ->
            case form2update msg of
                Just msg_ ->
                    Just (Update.ListUpdateMsg index msg_)
                Nothing ->
                    Nothing
        Form.ArrayMsg index msg ->
            case form2update msg of
                Just msg_ ->
                    Just (Update.ArrayUpdateIndexMsg index msg_)
                Nothing ->
                    Nothing
        Form.MaybeMsg msg ->
            Just (Update.MaybeUpdateMsg (form2update msg))
        Form.DictMsg key msg ->
            case (form2update msg, key) of
                (Just msg_, Just key_) ->
                    Just (Update.DictKeyMsg key_ msg_)
                (_, _) ->
                    Nothing
        Form.ResultMsg state msg ->
            case (state, form2update msg) of
                (Form.ErrForm , Just val) ->
                    Just (Update.ResultErrMsg val)
                (Form.OkForm, Just val) ->
                    Just (Update.ResultOkMsg val)
                _ -> 
                    Nothing
        Form.AttrMsg name msg ->
            case form2update msg of
                Just val ->
                    Just (Update.AttributeMsg name val)
                Nothing ->
                    Nothing
                    
            
        

                    