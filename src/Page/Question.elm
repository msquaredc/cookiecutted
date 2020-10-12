module Page.Question exposing (..)

import Session
import Page exposing (Page(..))
import Msg
import Viewer exposing (detailsConfig)
import Html exposing (Html, text, p)
import Material.LayoutGrid exposing (layoutGrid, inner, cell)
import Material.Typography as Typography exposing (typography)
import Material.TextField as TextField
import Material.Radio as Radio
import Material.Select as Select
import Material.Select.Item as SelectItem
import Material.FormField as FormField
import Viewer.EditableText as EditableText
import Type.Database.TypeMatching as Match
import Type.Database as Db
import Type.IO.Setter as Updater
import Type.Database.InputType as IT 
import Time exposing (Posix)
import Dict
import Viewer.OrderAwareList exposing (orderAwareList, OrderAware)
import Html exposing (u)

type alias Model =
    {
        id : String,
        short : String,
        long : String,
        list : String,
        drop : Maybe ITDrop
    }

type ITDrop =
    SA
    | LA
    | L

init : Db.Database -> String -> Model
init db id =
    let
        emptymodel = Model 
                        id
                        ""
                        ""
                        ""
                        Nothing
        q = Dict.get id db.questions
             |> Maybe.map .value
        it_id = Maybe.map .input_type q
        it = q
             |> Maybe.andThen (Db.question.viewer db)
             |> Maybe.map .input_type

    in 
    case (it, it_id) of 
        (Just (IT.ShortAnswer _), Just a )->
            {emptymodel | short = a, drop = Just SA}
        (Just (IT.LongAnswer _), Just a) ->
            {emptymodel | long = a, drop = Just LA}
        (Just (IT.List _ ), Just a) -> 
            {emptymodel | list = a, drop = Just L}
        _ ->
            emptymodel

    
        

page : Session.Session -> String -> ( Page.Page Model Msg.Msg, Cmd Msg.Msg )
page session id =
    let
        model =
            { session = session
            , page = init session.db id
            , view = view
            , toMsg = identity

            -- , header = Viewer.header
            , update = update

            --            , update = Page.liftupdate update
            }
    in
    ( Page model, Cmd.none )

update : Msg.Msg -> Page.Page Model Msg.Msg -> ( Page.Page Model Msg.Msg, Cmd Msg.Msg )
update message (Page model) =
    let
        oldmodel =
            model.page
        updatePage x = 
            Page {model| page = x}
        
        setInputTypeDb it = 
            let
                oldsession = model.session
               
                
                newdb n = {oldsession | db = n}
                olddb = oldsession.db
                newq n = newdb {olddb | questions = n}
            in
                newq <|
                    Dict.update 
                        oldmodel.id 
                        (
                           Maybe.map (
                               \i -> 
                                    let
                                        oldvalue = i.value
                                        newvalue n = {i | value = n}
                                        newquestion = {oldvalue | input_type = it}
                                    in
                                        newvalue newquestion
                           )
                        )
                        model.session.db.questions
           
    in
    case message of 
        Msg.Question msg ->
            case msg of
                Msg.SetInputType it ->
                    let
                        newsession = setInputTypeDb it
                    in
                        case it of
                         (IT.ShortAnswer t) -> 
                            (Page {model|page = {oldmodel|drop = Just SA, short = t}, session = newsession}, Cmd.none)
                         (IT.LongAnswer t) ->
                            (Page {model|page = {oldmodel|drop = Just LA, long = t}, session = newsession}, Cmd.none)
                         (IT.List t) ->
                            (Page {model|page = {oldmodel|drop = Just L, list = t}, session = newsession}, Cmd.none)
                Msg.Short msg_ ->
                    case oldmodel.drop of 
                        Just SA ->
                            let
                                oldshort = oldmodel.short
                                newshort = case msg_ of
                                            Msg.ShortLabel s ->
                                                {oldshort|label = Just s}
                                            Msg.ShortPlaceholder s ->
                                                {oldshort|placeholder = Just s}
                            in
                                (Page {model |session = setInputTypeDb <| IT.ShortAnswer newshort, page = {oldmodel | short = newshort }}, Cmd.none)
                        _->
                            (Page model, Cmd.none)
                Msg.Long msg_ ->
                    case oldmodel.drop of 
                        Just LA ->
                            let
                                oldlong = oldmodel.long
                                newlong = case msg_ of
                                            Msg.LongLabel s ->
                                                {oldlong|label = Just s}
                            in
                                (Page {model |session = setInputTypeDb <| IT.LongAnswer newlong, page = {oldmodel | long = newlong}}, Cmd.none)
                        _->
                            (Page model, Cmd.none)
                Msg.List msg_ ->
                    case oldmodel.drop of 
                        Just L ->
                            let
                                oldlist = oldmodel.list
                                newlist = case msg_ of
                                            Msg.SingleInput s ->
                                                {oldlist|singleInput = s}
                            in
                                (Page {model |session = setInputTypeDb <| IT.List newlist, page = {oldmodel | list = newlist}}, Cmd.none)
                                --(Page {model|page = {oldmodel | list = newlist}}, Cmd.none)
                        _->
                            (Page model, Cmd.none)
        _ ->
            (Page model, Cmd.none)

view : Page Model Msg.Msg -> Viewer.Details Msg.Msg
view (Page.Page model) =
    let
        db =
            model.session.db
        mbInfos =
            relatedData model.page.id db
    in
        { detailsConfig
                | title = toTitle model.page
                , user = model.session.user
                , body =
                    \_ -> [
                        case mbInfos of 
                            Just infos ->
                                layoutGrid [ Typography.typography ]
                                    [ inner [] <|
                                        [ cell [{- LG.span12 -}] <|
                                            [ Html.h1 [ Typography.headline5 ]
                                                [ text "Question"
                                                ]

                                            -- , p [][ text <| "Location:" ++ infos.location]
                                            , p [] [ text <| "Text: " {-++ viewStudy infos.study model.session.user-} ]
                                            , TextField.filled
                                                (TextField.config
                                                    |> TextField.setLabel (Just "Question text")
                                                    |> TextField.setValue (Just infos.text)
                                                    |> TextField.setOnInput (
                                                        \x ->
                                                            Match.setField
                                                                { kind = Db.QuestionType
                                                                , attribute = "text"
                                                                , setter = Updater.StringMsg
                                                                , id = model.page.id
                                                                , value = x
                                                                }
                                                    )
                                                )
                                            
                                            ] ++ viewInputTypeSelection model.page infos.input_type
                                        , cell [] <| viewSettings model.session.db model.page.id model.page
                                            
                                        ]
                                    ]
                            Nothing ->
                                text "got No Infos"
                        
                        
                    ]}

       

toTitle : Model -> String
toTitle _ =
    "Home ⧽ Question"


type alias RelatedData =
    { id : String
    , text : String
    , input_type : Maybe IT.InputType
    --, question : ( String, Maybe Db.Question)
    --, coding_questions : List (OrderAware Db.CodingQuestion)
    , created : Posix
    , creator : ( String, Maybe Db.User )
    , updated : Posix
    }


relatedData : String -> Db.Database -> Maybe RelatedData
relatedData id db =
    case Dict.get id db.questions of 
        Just timestampedQuestion ->
            let
                coding_questions =
                    {- List.sortBy (\( _, y ) -> y.index) <|-}
                        List.filter (\( _, y ) -> y.question == id) <|
                            List.map (\( x, y ) -> ( x, y.value )) <|
                                Dict.toList db.coding_questionnaries

                question =
                    timestampedQuestion.value
                
            in
            Just
                { id = id
                , text = question.text
                , input_type = Db.question.viewer db question |> Maybe.map .input_type
                --, question = ( questionary.study, Maybe.map .value <| Dict.get questionary.study db.studie)
                --, max_index = List.maximum <| List.map (\( _, x ) -> x.index) coding_questions
                --, coding_questions = orderAwareList coding_questions
                , created = Time.millisToPosix timestampedQuestion.created
                , creator = ( timestampedQuestion.creator, Maybe.map .value <| Dict.get timestampedQuestion.creator db.users )
                , updated = Time.millisToPosix timestampedQuestion.modified
                }

        Nothing ->
            Nothing

{- viewInputTypeSelection2 : Model -> IT.InputType -> Html Msg.Msg
viewInputTypeSelection2 model input_type = 
    Select.outlined
        (Select.config
            |> Select.setLabel (Just "Input Type")
            |> Select.setSelected (Just model.drop)
            |> Select.setOnChange (\drop -> Msg.Question <| Msg.SetInputType <|
                                                                case drop of
                                                                    Just SA -> IT.ShortAnswer model.short
                                                                    Just LA -> IT.LongAnswer model.long
                                                                    Just L -> IT.List model.list
                                                                    Nothing -> IT.ShortAnswer model.short)
        )
        (SelectItem.selectItem
            (SelectItem.config { value = Just SA })
            [ Html.text "Short Answer" ]
        )
        [ SelectItem.selectItem
            (SelectItem.config { value = Just LA })
            [ Html.text "Long Answer" ]
        ] -}

viewInputTypeSelection : Model -> Maybe IT.InputType -> List (Html Msg.Msg)
viewInputTypeSelection model mbit =
    List.map (\x -> Html.p [] [x])
    [
    FormField.formField
        (FormField.config
            |> FormField.setLabel (Just "Short Answer")
            |> FormField.setFor (Just "1")
            |> FormField.setOnClick ( Match.setField
                                            { kind = Db.QuestionType
                                            , attribute = "input_type"
                                            , setter = Updater.StringMsg
                                            , id = model.id
                                            , value = model.short
                                            })
--            |> FormField.setAttributes [ style "margin" "0 10px" ]
        )
        [ Radio.radio
            (Radio.config
                |> Radio.setChecked (model.drop == Just SA)
               |> Radio.setOnChange (Match.setField
                                            { kind = Db.QuestionType
                                            , attribute = "input_type"
                                            , setter = Updater.StringMsg
                                            , id = model.id
                                            , value = model.short
                                            })
            )
        
        ]
    , FormField.formField
        (FormField.config
            |> FormField.setLabel (Just "Long Answer")
            |> FormField.setFor (Just "2")
            |> FormField.setOnClick (Match.setField
                                            { kind = Db.QuestionType
                                            , attribute = "input_type"
                                            , setter = Updater.StringMsg
                                            , id = model.id
                                            , value = model.long
                                            })
--            |> FormField.setAttributes [ style "margin" "0 10px" ]
        )
        [ Radio.radio
            (Radio.config
                |> Radio.setChecked (model.drop == Just LA)
                |> Radio.setOnChange (Match.setField
                                            { kind = Db.QuestionType
                                            , attribute = "input_type"
                                            , setter = Updater.StringMsg
                                            , id = model.id
                                            , value = model.long
                                            })
            )
        
        ]
    , FormField.formField
        (FormField.config
            |> FormField.setLabel (Just "Multiple Choice")
            |> FormField.setFor (Just "3")
            |> FormField.setOnClick (Match.setField
                                            { kind = Db.QuestionType
                                            , attribute = "input_type"
                                            , setter = Updater.StringMsg
                                            , id = model.id
                                            , value = model.list
                                            })
--            |> FormField.setAttributes [ style "margin" "0 10px" ]
        )
        [ Radio.radio
            (Radio.config
                |> Radio.setChecked (model.drop == Just L)
                |> Radio.setOnChange (Match.setField
                                            { kind = Db.QuestionType
                                            , attribute = "input_type"
                                            , setter = Updater.StringMsg
                                            , id = model.id
                                            , value = model.short
                                            })
            )
        
        ]
    ]

viewSettings : Db.Database -> String -> Model -> List (Html Msg.Msg)
viewSettings db id model =
    let
        umessage attribute setter value= 
            Msg.CRUD <|
                Msg.Update <|
                    Updater.AttributeMsg "questions" <|
                        Updater.DictKeyMsg id <|
                            Updater.AttributeMsg "value" <|
                                Updater.AttributeMsg "input_type" <|
                                    Updater.AttributeMsg attribute <|
                                        setter value
    in
    case model.drop of 
        Just SA ->
            case Maybe.map .value (Dict.get model.short db.input_types) of
                Just (IT.ShortAnswer short) ->
                    [
                        TextField.filled
                            (TextField.config
                                |> TextField.setLabel (Just "Set the label")
                                |> TextField.setValue (short.label)
                                |> TextField.setOnInput (\x -> Match.setField
                                                            { kind = Db.InputTypeType
                                                            , attribute = "label"
                                                            , setter = Updater.StringMsg
                                                            , id = model.short
                                                            , value = x
                                                            })
                            )
                        , TextField.filled
                            (TextField.config
                                |> TextField.setLabel (Just "Set the placeholder")
                                |> TextField.setValue (short.placeholder)
                                |> TextField.setOnInput (\x -> Match.setField
                                                            { kind = Db.InputTypeType
                                                            , attribute = "placeholder"
                                                            , setter = Updater.StringMsg
                                                            , id = model.short
                                                            , value = x
                                                            })
                            )
                    ]
        Just LA ->
            [text "Long Answer settings"]
        Just L -> 
            [text "List Answer settings"]
        Nothing ->
            [text "You have not selected an Input type yet."]