module Page.Question exposing (..)

import Dict
import Html exposing (Html, p, text)
import Material.FormField as FormField
import Material.LayoutGrid exposing (cell, inner, layoutGrid)
import Material.Radio as Radio
import Material.TextField as TextField
import Material.Typography as Typography exposing (typography)
import Msg
import Page exposing (Page(..))
import Session
import Time exposing (Posix)
import Type.Database as Db
import Type.Database.InputType as IT
import Type.Database.TypeMatching as Match
import Type.IO.Setter as Updater
import Viewer exposing (detailsConfig)


type alias Model =
    { id : String
    , short : Maybe String
    , long : Maybe String
    , list : Maybe String
    }


init : Db.Database -> String -> Model
init db id =
    let
        emptymodel =
            Model
                id
                Nothing
                Nothing
                Nothing

        q =
            Dict.get id db.questions
                |> Maybe.map .value

        it_id =
            Maybe.map .input_type q

        it =
            q
                |> Maybe.andThen (Db.question.viewer db)
                |> Maybe.map .input_type
    in
    case ( it, it_id ) of
        ( Just (IT.ShortAnswer _), a ) ->
            { emptymodel | short = a }

        ( Just (IT.LongAnswer _), a ) ->
            { emptymodel | long = a }

        ( Just (IT.List _), a ) ->
            { emptymodel | list = a }

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
            Page { model | page = x }

        setInputTypeDb it =
            let
                oldsession =
                    model.session

                newdb n =
                    { oldsession | db = n }

                olddb =
                    oldsession.db

                newq n =
                    newdb { olddb | questions = n }
            in
            newq <|
                Dict.update
                    oldmodel.id
                    (Maybe.map
                        (\i ->
                            let
                                oldvalue =
                                    i.value

                                newvalue n =
                                    { i | value = n }

                                newquestion =
                                    { oldvalue | input_type = it }
                            in
                            newvalue newquestion
                        )
                    )
                    model.session.db.questions
    in
    case message of
        Msg.Question _ ->
            ( Page model, Cmd.none )

        {- case msg of
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
        -}
        _ ->
            ( Page model, Cmd.none )


view : Page Model Msg.Msg -> Viewer.Details Msg.Msg
view (Page.Page model) =
    let
        db =
            model.session.db

        mbInfos =
            relatedData model.page.id db

        moreInfos : String
        moreInfos =
            Dict.get model.page.id db.questions
                |> Maybe.map .value
                |> Maybe.andThen (Result.toMaybe << Db.question.toString "*")
                |> Maybe.withDefault "Nothing Found"
    in
    { detailsConfig
        | title = toTitle model.page
        , user = model.session.user
        , body =
            \_ ->
                [ text moreInfos
                , case mbInfos of
                    Just infos ->
                        layoutGrid [ Typography.typography ]
                            [ inner [] <|
                                [ cell [{- LG.span12 -}] <|
                                    [ Html.h1 [ Typography.headline5 ]
                                        [ text "Question"
                                        ]

                                    -- , p [][ text <| "Location:" ++ infos.location]
                                    , p []
                                        [ text <| "Text: "

                                        {- ++ viewStudy infos.study model.session.user -}
                                        ]
                                    , TextField.filled
                                        (TextField.config
                                            |> TextField.setLabel (Just "Question text")
                                            |> TextField.setValue (Just infos.text)
                                            |> TextField.setOnInput
                                                (\x ->
                                                    Match.setField
                                                        { kind = Db.QuestionType
                                                        , attribute = "text"
                                                        , setter = Updater.StringMsg
                                                        , id = model.page.id
                                                        , value = x
                                                        }
                                                )
                                        )
                                    ]
                                        ++ viewInputTypeSelection model.page infos.input_type
                                , cell [] <| viewSettings model.session.db model.page.id model.page infos.input_type
                                ]
                            ]

                    Nothing ->
                        text "got No Infos"
                ]
    }


toTitle : Model -> String
toTitle _ =
    "Home ⧽ Question"


type alias RelatedData =
    { id : String
    , text : String
    , input_type : ( String, Maybe IT.InputType )

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
                    {- List.sortBy (\( _, y ) -> y.index) <| -}
                    List.filter (\( _, y ) -> y.question == id) <|
                        List.map (\( x, y ) -> ( x, y.value )) <|
                            Dict.toList db.coding_questionnaries

                question =
                    timestampedQuestion.value
            in
            Just
                { id = id
                , text = question.text
                , input_type = ( question.input_type, Db.question.viewer db question |> Maybe.map .input_type )

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
           ]
-}


viewInputTypeSelection : Model -> ( String, Maybe IT.InputType ) -> List (Html Msg.Msg)
viewInputTypeSelection model ( id, _ ) =
    List.map (\x -> Html.p [] [ x ])
        [ FormField.formField
            (FormField.config
                |> FormField.setLabel (Just "Short Answer")
                |> FormField.setFor (Just "1")
                |> FormField.setOnClick
                    (case model.short of
                        Just short ->
                            Match.setField
                                { kind = Db.QuestionType
                                , attribute = "input_type"
                                , setter = Updater.StringMsg
                                , id = model.id
                                , value = short
                                }

                        Nothing ->
                            Msg.CRUD <|
                                Msg.CreateRandom (Db.InputTypeType Db.ShortKind)
                                    [ \x ->
                                        Match.setField
                                            { kind = Db.QuestionType
                                            , attribute = "input_type"
                                            , setter = Updater.StringMsg
                                            , id = model.id
                                            , value = x
                                            }
                                    ]
                    )
             --            |> FormField.setAttributes [ style "margin" "0 10px" ]
            )
            [ Radio.radio
                (Radio.config
                    |> Radio.setChecked (model.short == Just id)
                    |> Radio.setOnChange
                        (case model.short of
                            Just short ->
                                Match.setField
                                    { kind = Db.QuestionType
                                    , attribute = "input_type"
                                    , setter = Updater.StringMsg
                                    , id = model.id
                                    , value = short
                                    }

                            Nothing ->
                                Msg.CRUD <|
                                    Msg.CreateRandom (Db.InputTypeType Db.ShortKind)
                                        [ \x ->
                                            Match.setField
                                                { kind = Db.QuestionType
                                                , attribute = "input_type"
                                                , setter = Updater.StringMsg
                                                , id = model.id
                                                , value = x
                                                }
                                        ]
                        )
                )
            ]
        , FormField.formField
            (FormField.config
                |> FormField.setLabel (Just "Long Answer")
                |> FormField.setFor (Just "2")
                |> FormField.setOnClick
                    (case model.long of
                        Just long ->
                            Match.setField
                                { kind = Db.QuestionType
                                , attribute = "input_type"
                                , setter = Updater.StringMsg
                                , id = model.id
                                , value = long
                                }

                        Nothing ->
                            Msg.CRUD <|
                                Msg.CreateRandom (Db.InputTypeType Db.LongKind)
                                    [ \x ->
                                        Match.setField
                                            { kind = Db.QuestionType
                                            , attribute = "input_type"
                                            , setter = Updater.StringMsg
                                            , id = model.id
                                            , value = x
                                            }
                                    ]
                    )
             --            |> FormField.setAttributes [ style "margin" "0 10px" ]
            )
            [ Radio.radio
                (Radio.config
                    |> Radio.setChecked (model.long == Just id)
                    |> Radio.setOnChange
                        (case model.long of
                            Just long ->
                                Match.setField
                                    { kind = Db.QuestionType
                                    , attribute = "input_type"
                                    , setter = Updater.StringMsg
                                    , id = model.id
                                    , value = long
                                    }

                            Nothing ->
                                Msg.CRUD <|
                                    Msg.CreateRandom (Db.InputTypeType Db.LongKind)
                                        [ \x ->
                                            Match.setField
                                                { kind = Db.QuestionType
                                                , attribute = "input_type"
                                                , setter = Updater.StringMsg
                                                , id = model.id
                                                , value = x
                                                }
                                        ]
                        )
                )
            ]
        , FormField.formField
            (FormField.config
                |> FormField.setLabel (Just "Multiple Choice")
                |> FormField.setFor (Just "3")
                |> FormField.setOnClick
                    (case model.list of
                        Just list ->
                            Match.setField
                                { kind = Db.QuestionType
                                , attribute = "input_type"
                                , setter = Updater.StringMsg
                                , id = model.id
                                , value = list
                                }

                        Nothing ->
                            Msg.CRUD <|
                                Msg.CreateRandom (Db.InputTypeType Db.ListKind)
                                    [ \x ->
                                        Match.setField
                                            { kind = Db.QuestionType
                                            , attribute = "input_type"
                                            , setter = Updater.StringMsg
                                            , id = model.id
                                            , value = x
                                            }
                                    ]
                    )
             --            |> FormField.setAttributes [ style "margin" "0 10px" ]
            )
            [ Radio.radio
                (Radio.config
                    |> Radio.setChecked (model.list == Just id)
                    |> Radio.setOnChange
                        (case model.list of
                            Just list ->
                                Match.setField
                                    { kind = Db.QuestionType
                                    , attribute = "input_type"
                                    , setter = Updater.StringMsg
                                    , id = model.id
                                    , value = list
                                    }

                            Nothing ->
                                Msg.CRUD <|
                                    Msg.CreateRandom (Db.InputTypeType Db.ListKind)
                                        [ \x ->
                                            Match.setField
                                                { kind = Db.QuestionType
                                                , attribute = "input_type"
                                                , setter = Updater.StringMsg
                                                , id = model.id
                                                , value = x
                                                }
                                        ]
                        )
                )
            ]
        ]


viewSettings : Db.Database -> String -> Model -> ( String, Maybe IT.InputType ) -> List (Html Msg.Msg)
viewSettings db id model ( itid, mbit ) =
    let
        umessage attribute setter value =
            Msg.CRUD <|
                Msg.Update <|
                    Updater.AttributeMsg "questions" <|
                        Updater.DictKeyMsg id <|
                            Updater.AttributeMsg "value" <|
                                Updater.AttributeMsg "input_type" <|
                                    Updater.AttributeMsg attribute <|
                                        setter value

        moreInfo : String
        moreInfo =
            Maybe.map (IT.input_type.toString "*") mbit
                |> Maybe.andThen Result.toMaybe
                |> Maybe.withDefault ""
    in
    if model.short == Just itid then
        case Maybe.map .value <| Maybe.andThen (\x -> Dict.get x db.input_types) model.short of
            Just (IT.ShortAnswer short) ->
                [ text moreInfo
                , TextField.filled
                    (TextField.config
                        |> TextField.setLabel (Just "Set the label")
                        |> TextField.setValue short.label
                        |> TextField.setOnInput
                            (\x ->
                                Match.setField
                                    { kind = Db.InputTypeType Db.ShortKind
                                    , attribute = "label"
                                    , setter = \p -> (Updater.MaybeSetMsg (Just (Updater.StringMsg p)))
                                    , id = itid
                                    , value = x
                                    }
                            )
                    )
                , TextField.filled
                    (TextField.config
                        |> TextField.setLabel (Just "Set the placeholder")
                        |> TextField.setValue short.placeholder
                        |> TextField.setOnInput
                            (\x ->
                                Match.setField
                                    { kind = Db.InputTypeType Db.ShortKind
                                    , attribute = "placeholder"
                                    , setter = Updater.StringMsg
                                    , id = itid
                                    , value = x
                                    }
                            )
                    )
                ]

            _ ->
                [ text moreInfo, text "No Config found" ]

    else
        [ text moreInfo, text "You have not selected an Input type yet." ]
