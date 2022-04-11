module Page.Question exposing (Model, RelatedData, page)

import Dict
import Html exposing (Html, p, text)
import Material.Button as Button
import Material.FormField as FormField
import Material.LayoutGrid exposing (cell, inner, layoutGrid)
import Material.List as List
import Material.List.Item as ListItem exposing (ListItem)
import Material.Radio as Radio
import Material.Slider as Slider
import Material.Switch as Switch
import Material.TextField as TextField
import Material.Typography as Typography
import Msg
import Page exposing (Page(..))
import Session
import Time exposing (Posix)
import Type.Database as Db
import Type.Database.InputType as IT
import Type.Database.TypeMatching as Match
import Type.IO.Internal exposing (Id, box, unbox)
import Type.IO.Setter as Updater
import Viewer exposing (detailsConfig)


type alias Model =
    { id : Id Db.Question String
    , question : Maybe (Db.Timestamp Db.Question)
    , short : Maybe (Id IT.InputType String)
    , long : Maybe (Id IT.InputType String)
    , list : Maybe (Id IT.InputType String)
    , codingQuestionaryID : Maybe (Id Db.CodingQuestionary String)
    , codingQuestionary : Maybe (Db.Timestamp Db.CodingQuestionary)
    , codingQuestions : List ( String, Db.Timestamp Db.CodingQuestion )
    }


init : Db.Database -> Id Db.Question String -> Model
init db id =
    let
        emptyModel : Model
        emptyModel =
            Model
                id
                (Dict.get (unbox id) db.questions)
                Nothing
                Nothing
                Nothing
                (Maybe.map box cid)
                (Maybe.map Tuple.second coding_questionary)
                coding_questions

        cid =
            Maybe.map Tuple.first coding_questionary

        coding_questionary =
            Dict.filter (\_ cq -> cq.value.question == id) db.coding_questionnaries
                |> Dict.toList
                |> List.sortBy (\( _, cq ) -> cq.created)
                |> List.head

        coding_questions =
            Dict.filter (\_ cqq -> Just cqq.value.coding_questionary == Maybe.map box cid) db.coding_questions
                |> Dict.toList

        q =
            Dict.get (unbox id) db.questions
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
            { emptyModel | short = a }

        ( Just (IT.LongAnswer _), a ) ->
            { emptyModel | long = a }

        ( Just (IT.List _), a ) ->
            { emptyModel | list = a }

        _ ->
            emptyModel


page : Session.Session -> Id Db.Question String -> ( Page.Page Model Msg.Msg, Cmd Msg.Msg )
page session id =
    let
        model =
            { session = session
            , page = init session.db id
            , view = view
            , toMsg = identity
            , subscriptions = Sub.none

            -- , header = Viewer.header
            , update = update

            --            , update = Page.liftupdate update
            }
    in
    ( Page model, Cmd.none )


update : Msg.Msg -> Page.Page Model Msg.Msg -> ( Page.Page Model Msg.Msg, Cmd Msg.Msg )
update message (Page model) =
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
            Dict.get (unbox model.page.id) db.questions
                |> Maybe.map .value
                |> Maybe.andThen (Result.toMaybe << Db.question.toString "*")
                |> Maybe.withDefault "Nothing Found"
    in
    { detailsConfig
        | title = toTitle model.page
        , user = model.session.user
        , body =
            \_ ->
                [ case mbInfos of
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
                                            --|> TextField.setFullwidth True
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
                                , cell [] <| viewCodingQuestions model.page
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
    { id : Id Db.Question String
    , text : String
    , input_type : ( Id IT.InputType String, Maybe IT.InputType )

    --, question : ( String, Maybe Db.Question)
    --, coding_questions : List (OrderAware Db.CodingQuestion)
    , created : Posix
    , creator : ( Id Db.User String, Maybe Db.User )
    , updated : Posix
    }


relatedData : Id Db.Question String -> Db.Database -> Maybe RelatedData
relatedData id db =
    case Dict.get (unbox id) db.questions of
        Just timestampedQuestion ->
            let
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
                , creator = ( timestampedQuestion.creator, Maybe.map .value <| Dict.get (unbox timestampedQuestion.creator) db.users )
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


viewInputTypeSelection : Model -> ( Id IT.InputType String, Maybe IT.InputType ) -> List (Html Msg.Msg)
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
                                , value = unbox short
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
                                    , value = unbox short
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
                                , value = unbox long
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
                                    , value = unbox long
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
                                , value = unbox list
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
                                    , value = unbox list
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


viewCodingQuestions : Model -> List (Html Msg.Msg)
viewCodingQuestions model =
    let
        enabled =
            case model.codingQuestionary of
                Just cq ->
                    cq.value.enabled == True

                _ ->
                    False
    in
    [ FormField.formField
        (FormField.config
            |> FormField.setLabel (Just "Enable Coding")
        )
        [ Switch.switch
            (Switch.config
                |> Switch.setChecked enabled
                |> Switch.setOnChange
                    (case ( model.codingQuestionaryID, model.codingQuestionary ) of
                        ( Just cqid, Just cq ) ->
                            Match.setField
                                { kind = Db.CodingQuestionaryType
                                , attribute = "enabled"
                                , setter = Updater.BoolMsg
                                , id = cqid
                                , value = not cq.value.enabled
                                }

                        _ ->
                            Msg.CRUD <|
                                Msg.CreateRandom Db.CodingQuestionaryType
                                    [ \x ->
                                        Match.setField
                                            { kind = Db.CodingQuestionaryType
                                            , attribute = "question"
                                            , setter = Updater.StringMsg
                                            , id = box x
                                            , value = unbox model.id
                                            }
                                    , \x ->
                                        Match.setField
                                            { kind = Db.CodingQuestionaryType
                                            , attribute = "enabled"
                                            , setter = Updater.BoolMsg
                                            , id = box x
                                            , value = True
                                            }
                                    ]
                    )
            )
        ]
    , case List.map viewCodingQuestion model.codingQuestions of
        first :: rest ->
            List.list List.config
                first
                rest

        _ ->
            case model.codingQuestionaryID of
                Just _ ->
                    text "No Question yet!"

                Nothing ->
                    Html.div [] []
    , case model.codingQuestionaryID of
        Just cid ->
            Button.unelevated
                (Button.config
                    |> Button.setIcon (Just <| Button.icon "add")
                    |> Button.setOnClick
                        --Just <|
                        (Msg.CRUD <|
                            Msg.CreateRandom Db.CodingQuestionType
                                [ \x ->
                                    Match.setField
                                        { kind = Db.CodingQuestionType
                                        , attribute = "coding_questionary"
                                        , setter = Updater.StringMsg
                                        , id = box x
                                        , value = unbox cid
                                        }
                                ]
                        )
                )
                "Add"

        Nothing ->
            Html.div [] []
    ]


viewCodingQuestion : ( String, Db.Timestamp Db.CodingQuestion ) -> ListItem Msg.Msg
viewCodingQuestion ( id, cquestion ) =
    ListItem.listItem (ListItem.config |> ListItem.setOnClick (Msg.Follow Db.CodingQuestionType id))
        [ text cquestion.value.text ]


viewSettings : Db.Database -> Id Db.Question String -> Model -> ( Id IT.InputType String, Maybe IT.InputType ) -> List (Html Msg.Msg)
viewSettings db id model ( itid, mbit ) =
    if model.short == Just itid then
        case Maybe.map .value <| Maybe.andThen (\x -> Dict.get (unbox x) db.input_types) model.short of
            Just (IT.ShortAnswer short) ->
                [ TextField.filled
                    (TextField.config
                        |> TextField.setLabel (Just "Set the label")
                        |> TextField.setValue short.label
                        |> TextField.setOnInput
                            (\x ->
                                Match.setField
                                    { kind = Db.InputTypeType Db.ShortKind
                                    , attribute = "label"
                                    , setter = \p -> Updater.MaybeSetMsg (Just (Updater.StringMsg p))
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
                                    , setter = \p -> Updater.MaybeSetMsg (Just (Updater.StringMsg p))
                                    , id = itid
                                    , value = x
                                    }
                            )
                    )
                , TextField.filled
                    (TextField.config
                        |> TextField.setLabel (Just "Set the pattern")
                        |> TextField.setValue short.pattern
                        |> TextField.setOnInput
                            (\x ->
                                Match.setField
                                    { kind = Db.InputTypeType Db.ShortKind
                                    , attribute = "pattern"
                                    , setter = \p -> Updater.MaybeSetMsg (Just (Updater.StringMsg p))
                                    , id = itid
                                    , value = x
                                    }
                            )
                    )
                , p []
                    [ text <| "Min Length: " ++ (Maybe.withDefault "0" <| Maybe.map String.fromInt short.minLength)
                    , Slider.slider
                        (Slider.config
                            |> (\x ->
                                    case Maybe.map toFloat short.minLength of
                                        Just minLength ->
                                            Slider.setValue minLength x

                                        Nothing ->
                                            x
                               )
                            --                        |> Slider.setMax (Maybe.map toFloat short.maxLength)
                            |> Slider.setOnInput
                                (\x ->
                                    Match.setField
                                        { kind = Db.InputTypeType Db.ShortKind
                                        , attribute = "minLength"
                                        , setter = \p -> Updater.MaybeSetMsg (Just (Updater.IntMsg <| round p))
                                        , id = itid
                                        , value = x
                                        }
                                )
                        )
                    ]
                , p []
                    [ text <| "Max Length: " ++ (Maybe.withDefault "100" <| Maybe.map String.fromInt short.maxLength)
                    , Slider.slider
                        (Slider.config
                            |> (\x ->
                                    case Maybe.map toFloat short.maxLength of
                                        Just minLength ->
                                            Slider.setValue minLength x

                                        Nothing ->
                                            x
                               )
                            --                        |> Slider.setMin (Maybe.map toFloat short.minLength)
                            |> Slider.setOnInput
                                (\x ->
                                    Match.setField
                                        { kind = Db.InputTypeType Db.ShortKind
                                        , attribute = "maxLength"
                                        , setter = \p -> Updater.MaybeSetMsg (Just (Updater.IntMsg <| round p))
                                        , id = itid
                                        , value = x
                                        }
                                )
                        )
                    ]
                ]

            _ ->
                [ text "No Config found" ]

    else if model.long == Just itid then
        case Maybe.map .value <| Maybe.andThen (\x -> Dict.get (unbox x) db.input_types) model.long of
            Just (IT.LongAnswer long) ->
                [ TextField.filled
                    (TextField.config
                        |> TextField.setLabel (Just "Set the label")
                        |> TextField.setValue long.label
                        |> TextField.setOnInput
                            (\x ->
                                Match.setField
                                    { kind = Db.InputTypeType Db.ShortKind
                                    , attribute = "label"
                                    , setter = \p -> Updater.MaybeSetMsg (Just (Updater.StringMsg p))
                                    , id = itid
                                    , value = x
                                    }
                            )
                    )
                , TextField.filled
                    (TextField.config
                        |> TextField.setLabel (Just "Set the placeholder")
                        |> TextField.setValue long.placeholder
                        |> TextField.setOnInput
                            (\x ->
                                Match.setField
                                    { kind = Db.InputTypeType Db.ShortKind
                                    , attribute = "placeholder"
                                    , setter = \p -> Updater.MaybeSetMsg (Just (Updater.StringMsg p))
                                    , id = itid
                                    , value = x
                                    }
                            )
                    )
                , TextField.filled
                    (TextField.config
                        |> TextField.setLabel (Just "Set the pattern")
                        |> TextField.setValue long.pattern
                        |> TextField.setOnInput
                            (\x ->
                                Match.setField
                                    { kind = Db.InputTypeType Db.ShortKind
                                    , attribute = "pattern"
                                    , setter = \p -> Updater.MaybeSetMsg (Just (Updater.StringMsg p))
                                    , id = itid
                                    , value = x
                                    }
                            )
                    )
                , p []
                    [ text <| "Min Length: " ++ (Maybe.withDefault "0" <| Maybe.map String.fromInt long.minLength)
                    , Slider.slider
                        (Slider.config
                            |> (\x ->
                                    case Maybe.map toFloat long.minLength of
                                        Just minLength ->
                                            Slider.setValue minLength x

                                        Nothing ->
                                            x
                               )
                            --                        |> Slider.setMax (Maybe.map toFloat short.maxLength)
                            |> Slider.setOnInput
                                (\x ->
                                    Match.setField
                                        { kind = Db.InputTypeType Db.ShortKind
                                        , attribute = "minLength"
                                        , setter = \p -> Updater.MaybeSetMsg (Just (Updater.IntMsg <| round p))
                                        , id = itid
                                        , value = x
                                        }
                                )
                        )
                    ]
                , p []
                    [ text <| "Max Length: " ++ (Maybe.withDefault "100" <| Maybe.map String.fromInt long.maxLength)
                    , Slider.slider
                        (Slider.config
                            |> (\x ->
                                    case Maybe.map toFloat long.maxLength of
                                        Just minLength ->
                                            Slider.setValue minLength x

                                        Nothing ->
                                            x
                               )
                            --                        |> Slider.setMin (Maybe.map toFloat short.minLength)
                            |> Slider.setOnInput
                                (\x ->
                                    Match.setField
                                        { kind = Db.InputTypeType Db.ShortKind
                                        , attribute = "maxLength"
                                        , setter = \p -> Updater.MaybeSetMsg (Just (Updater.IntMsg <| round p))
                                        , id = itid
                                        , value = x
                                        }
                                )
                        )
                    ]
                ]

            _ ->
                [ text "No Config found" ]

    else if model.list == Just itid then
        case Maybe.map .value <| Maybe.andThen (\x -> Dict.get (unbox x) db.input_types) model.list of
            Just (IT.List _) ->
                [ text "Boxes or Radio?"
                , FormField.formField
                    (FormField.config
                        |> FormField.setLabel (Just "Radio Button")
                    )
                    [ Radio.radio Radio.config ]
                , FormField.formField
                    (FormField.config
                        |> FormField.setLabel (Just "Checkbox")
                    )
                    [ Radio.radio Radio.config ]
                ]

            _ ->
                [ text "No Config found" ]

    else
        [ text "You have not selected an Input type yet." ]
