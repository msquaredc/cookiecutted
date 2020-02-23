module Type.IO.Form exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import Html.Events 
import Json.Decode exposing (map)
import List.Extra
import Material.Checkbox as Checkbox exposing (checkboxConfig)
import Material.TextField as TextField exposing (textFieldConfig)
import Maybe.Extra


type UpdateMsg
    = IntMsg (Maybe Int)
    | StringMsg (Maybe String)
    | FloatMsg (Maybe Float)
    | BoolMsg
    | MaybeMsg UpdateMsg
    | ListMsg Int UpdateMsg
    | DictMsg (Maybe String) UpdateMsg
    | ResultMsg ResultState UpdateMsg
    | ArrayMsg Int UpdateMsg
    | AttrMsg String UpdateMsg


type ResultState
    = ErrForm
    | OkForm


type alias FormAcc full msg =
    { forms : full -> List (Html.Html msg)
    }

type alias Form kind msg =
    String -> (UpdateMsg -> msg) -> kind -> String -> List (Html.Html msg)




view : String -> (UpdateMsg -> msg) -> Form kind msg -> kind -> String -> List (Html.Html msg)
view name callback form value acc =
    form name callback value acc



--|> Maybe.withDefault (Html.text "")


int : Form Int msg
int name callback kind _ =
    [
        TextField.textField
            { textFieldConfig
                | value = String.fromInt kind
                , onInput = Just (callback << IntMsg << String.toInt)
                , label = Just (name)
            }
    ]


-- int : Form Int (TextField.TextFieldConfig msg) msg
-- int =
--     { config = \callback kind ->
--         Just
--             { textFieldConfig
--                 | value = String.fromInt kind
--                 , onChange = Just (callback << IntMsg << String.toInt)
--             }
--     , view = TextField.textField
--     }


string2 : Form String msg
string2 name callback kind label =
    Debug.log ("Kind: " ++ kind)
    [
        TextField.textField
                { textFieldConfig
                    | label = Just name
                    , value = kind
                    , onInput = Just (callback << StringMsg << Just )
                }
    ]

string : Form String msg
string name callback kind label =
    Debug.log ("Kind: " ++ kind)
    [
        Html.input 
            [Html.Attributes.value kind
            , Html.Events.onInput (callback << StringMsg << Just )
            , Html.Attributes.placeholder name
            ][]
    ]


-- string : Form String (TextField.TextFieldConfig msg) msg
-- string  =
--     { config = \callback kind ->
--         Just
--             { textFieldConfig
--                 | value = kind
--                 , onChange = Just (callback << StringMsg << Just)
--             }
--     , view = TextField.textField
--     }


float : Form Float msg
float name callback kind _ =
    [
        TextField.textField
            { textFieldConfig
                | value = String.fromFloat kind
                , onInput = Just (callback << FloatMsg << String.toFloat)
                , label = Just name 
            }
    ]



-- float : Form Float (TextField.TextFieldConfig msg) msg
-- float =
--     { config = \callback kind ->
--         Just
--             { textFieldConfig
--                 | value = String.fromFloat kind
--                 , onChange = Just (callback << FloatMsg << String.toFloat)
--             }
--     , view = TextField.textField
--     }


bool : Form Bool msg
bool _ callback kind _ =
    let
        bool2state state =
            case state of
                Just True ->
                    Checkbox.Checked

                Just False ->
                    Checkbox.Unchecked

                Nothing ->
                    Checkbox.Indeterminate
    in
    [
        Checkbox.checkbox
            { checkboxConfig
                | state = bool2state <| Just kind
                , onChange = Just (callback BoolMsg)
            }
    ]


-- bool : Form Bool (Checkbox.CheckboxConfig msg) msg
-- bool =
--     let
--         bool2state state =
--             case state of
--                 Just True ->
--                     Checkbox.Checked
--                 Just False ->
--                     Checkbox.Unchecked
--                 Nothing ->
--                     Checkbox.Indeterminate
--     in
--     { config = \callback kind ->
--         Just
--             { checkboxConfig
--                 | state = bool2state <| Just kind
--                 , onChange = Just (callback BoolMsg)
--             }
--     , view = Checkbox.checkbox
--     }


maybe : Form a msg -> Form (Maybe a) msg
maybe old name callback kind acc =
    let
        new =
            Maybe.map (\x -> old name (callback << MaybeMsg) x acc) kind
    in
        new
        |> Maybe.withDefault []




-- list : Form a b msg -> Form (List a) (List b) msg
-- list old =
--     {
--         view = \x -> List.map old.view x
--                |> Html.div []
--         , config = \callback kind ->
--                 List.indexedMap (\index instance -> old.config (callback << ListMsg index) instance) kind
--                 |> Maybe.Extra.combine
--     }


list : Form a msg -> Form (List a) msg
list old name callback kind acc=
    let
        new =
            List.indexedMap (\index instance -> old name (callback << ListMsg index) instance rest) kind
        (parsedIndex,rest) = parseHeadTail acc
    in
         if parsedIndex == "*" then
            new
            |> List.concat
        else
        String.toInt parsedIndex
        |> Maybe.andThen (\x -> List.Extra.getAt x new)
        |> Maybe.withDefault []



{-
   list : List (Form a b msg) -> Form a b msg
   list old callback kind =
       List.indexedMap (\index x -> (x.config (callback << ListMsg index) kind, x.view )) old


       --List.indexedMap (\index value -> old (callback << ListMsg index) value) kind


       -- {
       --     config =
       --                 List.indexedMap (\index instance -> old.config (callback << ListMsg index) instance) kind
       --                 |> Maybe.Extra.combine
       --     , view = \x -> Html.div [] (List.map old.view x)

       -- }
-}
-- dict : (comparable -> String) -> Form a b msg -> Form (Dict comparable a) (Dict comparable b) msg
-- dict keySerializer old =
--     {
--         view = \x -> Dict.values x
--                |> List.map old.view
--                |> Html.div [],
--         config = \callback kind ->
--                 Dict.toList kind
--                 |> List.map (\(key,value) -> (Just key, old.config (callback << DictMsg (keySerializer key)) value))
--                 |> Maybe.Extra.traverse combine_tuple
--                 |> Maybe.map Dict.fromList
--     }


dict : (comparable -> Maybe String) -> Form a msg -> Form (Dict comparable a) msg
dict keySerializer old name callback kind acc =
    let
        new =
            Dict.map (\key instance -> old name (callback << DictMsg (keySerializer key)) instance rest) kind
        (parsedkey,rest) = parseHeadTail acc
    in
        if parsedkey == "*" then
            Dict.keys new
            |> List.filterMap (\x -> Dict.get x new)
            |> List.concat
        else
            Dict.keys new
                |> List.filter
                    (\x ->
                            keySerializer x
                            |> Maybe.map (\y -> y == parsedkey)
                            |> Maybe.withDefault False
                    )
                |> List.filterMap (\x -> Dict.get x new)
                |> List.concat



-- dict : (comparable -> String) -> Form a b msg -> Form (Dict comparable a) (Dict comparable b) msg
-- dict keySerializer old =
--     {
--         config = \callback kind ->
--             Dict.toList kind
--             |> List.map (\(key,value) -> (Just key, old.config (callback << DictMsg (keySerializer key)) value))
--             |> Maybe.Extra.traverse combine_tuple
--             |> Maybe.map Dict.fromList
--         ,view = \x -> Dict.values x
--                       |> List.map old.view
--                       |> Html.div []
--   }
{- Dict.toList kind
   |> List.map (\(key,value) -> (key, old (callback << DictMsg (keySerializer key)) value))
   |> List.map (\(a, b) -> ((a,b.config),(a,b.view)))
   |> List.foldr (\( a, b ) ( c, d ) -> ( a :: c, b :: d )) ( [], [] )
   |> (\(a, b)-> (Maybe.Extra.traverse combine_tuple a, b))
   |> (\(a, b)-> (Dict.fromList a, Dict.fromList b))
   |> (\(a, b) -> {config=Just a, view = b})
-}


combine_tuple : ( Maybe a, Maybe b ) -> Maybe ( a, b )
combine_tuple old =
    case old of
        ( Just a, Just b ) ->
            Just ( a, b )

        _ ->
            Nothing



{- Dict.toList kind
   |> List.map (\(key,value) -> (Just key, old.config (callback << DictMsg (keySerializer key)) value))
   |> Maybe.Extra.traverse combine_tuple
   |> Maybe.map Dict.fromList

-}
-- result : Form err b msg  -> Form a c msg -> Form (Result err a) (Result b c) msg
-- result err val =
--     {
--         view = \config -> case config of
--             Ok form ->
--                 val.view form
--             Err form  ->
--                 err.view form
--         , config = \callback kind ->
--             case kind of
--                 Ok config ->
--                     val.config (callback << ResultMsg OkForm) config
--                     |> Maybe.map Ok
--                 Err config->
--                     err.config (callback << ResultMsg ErrForm) config
--                     |> Maybe.map Err
--     }


result : Form err msg -> Form a msg -> Form (Result err a) msg
result err val name callback kind acc =
    case kind of
        Ok v ->
           val name (callback << ResultMsg OkForm) v acc
        Err v ->
            err name (callback << ResultMsg ErrForm) v acc
            
    



{- let
       new = Result.map (val (callback << ResultMsg OkForm)) kind
             |> Result.mapError (err (callback << ResultMsg ErrForm))

   in
       case new of
           Ok v ->
               {
                   view = v.view
                   , config = v.config
               }
           Err v ->
               {
                   view = .view (err (callback << ResultMsg ErrForm) v),
                   config = .config (err (callback << ResultMsg ErrForm) v)
               }
-}
-- array : Form a b msg-> Form (Array a) (Array b) msg
-- array old =
--     {
--         view = \x -> Array.map old.view x
--                 |> Array.toList
--                 |> Html.div []
--         , config = \callback kind ->
--                 Array.indexedMap (\index instance -> old.config (callback << ArrayMsg index) instance) kind
--                 |> Maybe.Extra.combineArray
--     }


array : Form a msg -> Form (Array a) msg
array old name callback kind acc =
    let
        new =
            Array.indexedMap (\index instance -> old name (callback << ArrayMsg index) instance rest) kind
        (parsedIndex,rest) = parseHeadTail acc
    in
        if parsedIndex == "*" then
            new
            |> Array.toList
            |> List.concat
        else
        String.toInt parsedIndex
        |> Maybe.andThen (\x -> Array.get x new)
        |> Maybe.withDefault []
        


entity : Form a msg
entity name callback kind acc =
    []


attribute : String -> (c -> a) -> Form a msg -> Form (c) msg -> Form c msg
attribute name getter childform parentform label callback kind acc =
        let
            ( head, tail ) =
                parseHeadTail acc
            newname = (label ++ "." ++ name)
        in
        if name == head then
            childform newname (callback<<AttrMsg head) (getter kind) tail

        else
            if head == "*" then
                parentform label callback kind acc ++ childform newname (callback<<AttrMsg head) (getter kind) tail
            else
            parentform label callback kind acc
    -- { view = \config -> (parentform callback kind).view config
    -- , config =
    --     { forms = \full -> (parentform callback full).config.forms full ++ [ view (callback << AttrMsg name) childform (getter full) ]
    --     }
    -- }




reference : String -> (c -> a) -> Form a msg -> Form (c) msg -> Form c msg
reference = attribute

references : String -> (c -> (List a)) -> Form (List a) msg -> Form (c) msg -> Form c msg
references name getter childform parentform label callback kind acc =
    let
        ( head, tail ) =
            parseHeadTail acc
        newname = (label ++ "." ++ name)
    in
    if name == head then
        childform newname (callback<<AttrMsg head) (getter kind) tail

    else
        parentform newname callback kind acc

substruct : String -> (c -> a) -> Form a msg -> Form (c) msg -> Form c msg
substruct name getter childform parentform label callback kind acc = 
        let
            ( head, tail ) =
                parseHeadTail acc
            newname = (label ++ "." ++ name)
        in
        if name == head then
            childform newname (callback<<AttrMsg head) (getter kind) tail

        else
            if head == "*" then
                parentform newname callback kind acc ++ childform newname (callback<<AttrMsg head) (getter kind) tail
            else
            parentform label callback kind acc


parseHeadTail : String -> ( String, String )
parseHeadTail accessor =
    let
        index =
            String.split "." accessor
                |> List.head
                |> Maybe.withDefault ""

        rest =
            String.split "." accessor
                |> List.tail
                |> Maybe.map (String.join ".")
                |> Maybe.withDefault ""
    in
    ( index, rest )