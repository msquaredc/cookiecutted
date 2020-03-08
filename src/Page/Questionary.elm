module Page.Questionary exposing (Model, defaultFokus, init, page, update, view)

--import Browser

import Dict
import Html exposing (Html, div, p, text)
import Html.Attributes
import Html.Events exposing (onClick)
import Identicon exposing (identicon)
import List.Extra
import Material.Button exposing (buttonConfig)
import Material.Card as Card exposing (cardActionButton, cardActionIcon, cardActions, cardBlock, cardConfig, cardPrimaryActionConfig)
import Material.Checkbox as Checkbox exposing (checkboxConfig)
import Material.IconButton exposing (iconButtonConfig)
import Material.LayoutGrid as LG exposing (layoutGrid, layoutGridCell, layoutGridInner)
import Material.List exposing (list, listConfig, listItem, listItemConfig, listItemGraphic)
import Material.Radio as Radio exposing (radioConfig)
import Material.Select as Select exposing (selectConfig, selectOptionConfig)
import Material.TextArea as TextArea exposing (textAreaConfig)
import Material.TextField as TextField exposing (textFieldConfig)
import Material.Typography as Typography
import Msg
import Page exposing (Page(..))
import Session
import Time exposing (Posix)
import Type.Database as Db
import Type.Database.InputType as IT
import Type.Database.TypeMatching as Match
import Type.IO.Form as Form
import Type.IO.Setter as Updater
import Viewer exposing (detailsConfig)



{-
   This is a page with subpages. You can change the behaviour depending on the subpage path!
-}
-- MODEL


type alias Model =
    { id : String
    , focus : Fokus
    }


type alias Fokus =
    { activeQuestion : Maybe String
    , titleFocused : Bool
    }


defaultFokus : Fokus
defaultFokus =
    Fokus Nothing False



-- INIT


init : String -> Fokus -> Model
init a b =
    Model a b


page : Session.Session -> String -> Fokus -> ( Page.Page Model Msg.Msg, Cmd Msg.Msg )
page session id focus =
    let
        model =
            { session = session
            , page = init id focus
            , view = view
            , toMsg = identity

            -- , header = Viewer.header
            , update = update

            --            , update = Page.liftupdate update
            }
    in
    ( Page model, Cmd.none )



-- UPDATE


update : Msg.Msg -> Page.Page Model Msg.Msg -> ( Page.Page Model Msg.Msg, Cmd Msg.Msg )
update message (Page model) =
    let
        oldmodel =
            model.page
    in
    case message of
        Msg.Questionary msg ->
            case msg of
                Msg.CurrentQuestionSelected selection ->
                    let
                        newfocus =
                            { oldfocus
                                | activeQuestion = selection
                                , titleFocused = False
                            }

                        oldfocus =
                            oldmodel.focus

                        newmodel =
                            { oldmodel | focus = newfocus }
                    in
                    ( Page { model | page = newmodel }, Cmd.none )

                Msg.FocusTitle ->
                    let
                        newfocus =
                            { oldfocus
                                | activeQuestion = Nothing
                                , titleFocused = True
                            }

                        oldfocus =
                            oldmodel.focus

                        newmodel =
                            { oldmodel | focus = newfocus }
                    in
                    ( Page { model | page = newmodel }, Cmd.none )

                Msg.LooseFocus ->
                    let
                        newmodel =
                            { oldmodel | focus = defaultFokus }
                    in
                    ( Page { model | page = newmodel }, Cmd.none )
                
                
        _ ->
            ( Page model, Cmd.none )



-- VIEW


view : Page Model Msg.Msg -> Viewer.Details Msg.Msg
view (Page.Page model) =
    let
        db =
            model.session.db

        mbInfos =
            relatedData model.page.id db
    in
    case mbInfos of
        Just infos ->
            { detailsConfig
                | title = toTitle model.page
                , user = model.session.user
                , body = \_ ->
                    [ layoutGrid [ Typography.typography ]
                        [ layoutGridInner [] <|
                            [ layoutGridCell [ LG.span12 ]
                                [ Html.h1 [ Typography.headline5 ]
                                    [ editableText
                                        model.page.focus.titleFocused
                                        (Msg.Questionary Msg.FocusTitle)
                                        (\x -> Msg.Questionary Msg.LooseFocus)
                                        infos.name
                                      <|
                                        \x ->
                                            Match.setField
                                                { kind = Db.QuestionaryType
                                                , attribute = "name"
                                                , setter = Updater.StringMsg
                                                , id = model.page.id
                                                , value = x
                                                }
                                    ]

                                -- , p [][ text <| "Location:" ++ infos.location]
                                , p [] [ text <| "Study: " ++ viewStudy infos.study model.session.user ]
                                ]
                            ]
                                ++ List.map (\x -> layoutGridCell [] [ viewQuestionCard db model.page.focus.activeQuestion x ]) infos.questions
                                ++ [ layoutGridCell []
                                        [ list listConfig <|
                                            [ listItem
                                                { listItemConfig
                                                    | onClick =
                                                        Just <|
                                                            Msg.CRUD <|
                                                                Msg.CreateRandom Db.QuestionType
                                                                    [ \x ->
                                                                        Match.setField
                                                                            { kind = Db.QuestionType
                                                                            , attribute = "questionary"
                                                                            , setter = Updater.StringMsg
                                                                            , id = x
                                                                            , value = infos.id
                                                                            }
                                                                    , \x ->
                                                                        Match.setField
                                                                            { kind = Db.QuestionType
                                                                            , attribute = "index"
                                                                            , setter = Updater.IntMsg
                                                                            , id = x
                                                                            , value = Maybe.withDefault 0 <| Maybe.map ((+) 1) infos.max_index
                                                                            }
                                                                    ]
                                                }
                                                [ Html.h3 [ Typography.headline3, Html.Attributes.style "justify-content" "center" ] [ text "+" ] ]
                                            ]
                                        ]

                                   -- , layoutGridCell [][
                                   --     Html.h1 [ Typography.headline5 ] [ text "Questionnaries" ]
                                   --     , viewList infos.questionnaries (Msg.Follow Db.QuestionaryType)
                                   --     , unelevatedButton
                                   --         {buttonConfig| icon = Just "add"
                                   --                      , onClick =  }
                                   --         "Add"
                                   -- ]
                                   ]
                        ]
                    ]
            }

        Nothing ->
            { detailsConfig
                | title = toTitle model.page
                , user = model.session.user
                , body = \_ ->
                    [ layoutGrid []
                        [ layoutGridInner []
                            [ layoutGridCell []
                                [ Html.h1 [ Typography.headline5 ] [ text <| "Questionary not Found" ]
                                ]
                            ]
                        ]
                    ]
            }


editableText : Bool -> Msg.Msg -> (String -> Msg.Msg) -> String -> (String -> Msg.Msg) -> Html Msg.Msg
editableText active activator deactivator value callback =
    if active then
        -- list {listConfig | nonInteractive = True}
        --     [ listItem listItemConfig
        --         [
        TextField.textField
            { textFieldConfig
                | value = value
                , onInput = Just callback
                , onChange = Just deactivator
                , label = Nothing
                , outlined = True

                -- , fullwidth = True
                , additionalAttributes = [ Typography.headline5 ]
            }
        --     ]
        -- ]

    else
        -- list {listConfig | nonInteractive = True }
        --     [ listItem {listItemConfig | onClick = Just activator}
        --         [
        div [ Typography.headline5, onClick <| activator ] [ text value ]



--     ]
-- ]


viewQuestionCard : Db.Database -> Maybe String -> OrderAware Db.Question -> Html Msg.Msg
viewQuestionCard db mbCur {id, value, previous, next} =
    let
        setMsg x callback =
            Match.setField
                { kind = Db.QuestionType
                , attribute = "input_type"
                , setter = \y -> Updater.Custom y callback
                , value = id
                , id = x
                }
        question = value
    in
    if mbCur == Just id then
        Card.card cardConfig
            { blocks =
                [ cardBlock <|
                    Html.div [ Html.Attributes.style "padding" "1rem" ]
                        [ Result.withDefault (div [] []) <| Match.forms id Db.QuestionType "text" db <| wideTextForm Nothing ]
                , cardBlock <|
                    Html.div [ Html.Attributes.style "padding" "1rem" ]
                        [ Select.outlinedSelect
                            { selectConfig
                                | label = "Question Type"
                                , value = Just (IT.toString question.input_type)
                                , onChange = Just (\x -> setMsg x Nothing)
                            }
                          <|
                            List.map
                                (\x ->
                                    Select.selectOption
                                        { selectOptionConfig | value = IT.toString x }
                                        [ text <| IT.toString x ]
                                )
                                IT.inputTypes
                        ]
                , cardBlock <|
                    Html.div [ Html.Attributes.style "padding" "1rem" ]
                        [ viewInputTypeActive question.input_type <| setMsg (IT.toString question.input_type) ]
                ]
            , actions =
                Just <|
                    cardActions
                        { buttons =
                            [ cardActionButton buttonConfig
                                "Visit"
                            ]
                        , icons =
                            (case previous of
                                Just prev ->
                                    [cardActionIcon {iconButtonConfig | onClick = Just <| Msg.CRUD <| Msg.SwapAttributes Db.QuestionType (prev, id) "index" }
                                    "arrow_upward"]
                                Nothing ->
                                    []
                            )
                            ++ 
                            (case next of
                                Just post ->
                                    [cardActionIcon {iconButtonConfig | onClick = Just <| Msg.CRUD <| Msg.SwapAttributes Db.QuestionType (post, id) "index" }
                                    "arrow_downward"]
                                Nothing ->
                                    []
                            )
                        }
            }

    else
        Card.card cardConfig
            { blocks =
                Card.cardPrimaryAction
                    { cardPrimaryActionConfig
                        | onClick = Just <| Msg.Questionary <| Msg.CurrentQuestionSelected <| Just id
                    }
                    [ cardBlock <|
                        div [ Html.Attributes.style "padding" "1rem", Typography.headline6 ]
                            [ text question.text ]
                    , cardBlock <|
                        Html.div [ Html.Attributes.style "padding" "1rem" ]
                            [ viewInputTypePassive question.input_type ]
                    ]
            , actions =
                Just <|
                    cardActions
                        { buttons =
                            [ cardActionButton buttonConfig
                                "Visit"
                            ]
                        , icons =
                            (case previous of
                                Just prev ->
                                    [cardActionIcon iconButtonConfig
                                    "arrow_upward"]
                                Nothing ->
                                    []
                            )
                            ++ 
                            (case next of
                                Just post ->
                                    [cardActionIcon iconButtonConfig
                                    "arrow_downward"]
                                Nothing ->
                                    []
                            )
                        }
            }


viewInputTypeActive : IT.InputType -> (Maybe Updater.Msg -> Msg.Msg) -> Html Msg.Msg
viewInputTypeActive kind callback =
    case kind of
        IT.ShortAnswer config ->
            TextField.textField
                { textFieldConfig
                    | value = ""
                    , onInput = Nothing
                    , label = Nothing
                    , outlined = True
                }

        IT.LongAnswer config ->
            TextArea.textArea
                { textAreaConfig
                    | label = Nothing
                    , value = ""
                    , onInput = Nothing
                    , rows = Just 4
                    , cols = Just 20
                }

        IT.List config ->
            list { listConfig | nonInteractive = True } <|
                List.indexedMap
                    (\index x ->
                        listItem listItemConfig
                            [ viewSingleInputType config.singleInput
                            , TextField.textField
                                { textFieldConfig
                                    | value = x
                                    , onInput =
                                        Just <|
                                            \y ->
                                                callback <|
                                                    Just
                                                        (Updater.AttributeMsg "choices" <|
                                                            Updater.ListMixedUpdate index <|
                                                                Updater.StringMsg y
                                                        )
                                    , label = Nothing
                                    , outlined = True
                                    , placeholder = Just "Add a question"
                                }
                            ]
                    )
                    (config.choices ++ [ "" ])



-- _ ->
--     div [] []


viewInputTypePassive : IT.InputType -> Html msg
viewInputTypePassive kind =
    case kind of
        IT.ShortAnswer config ->
            TextField.textField
                { textFieldConfig
                    | value = ""
                    , onInput = Nothing
                    , label = Nothing
                    , outlined = True
                }

        IT.LongAnswer config ->
            TextArea.textArea
                { textAreaConfig
                    | label = Nothing
                    , value = ""
                    , onInput = Nothing
                    , rows = Just 4
                    , cols = Just 20
                }

        IT.List config ->
            list { listConfig | nonInteractive = True } <|
                List.indexedMap
                    (\index x ->
                        listItem listItemConfig
                            [ viewSingleInputType config.singleInput
                            , text x
                            ]
                    )
                    config.choices


viewSingleInputType : IT.SingleInputType -> Html msg
viewSingleInputType kind =
    case kind of
        IT.Box ->
            Checkbox.checkbox checkboxConfig

        IT.Radio ->
            Radio.radio radioConfig



-- Db.List Db.Radio _ ->
--     "Multiple Choice"
-- Db.List Db.Box _ ->
--     "Boxes"
-- Db.DropDown _ ->
--     "DropDown Menu"
-- Db.LinearScale _ ->
--     "Linear Scale"
-- Db.Matrix Db.Radio _ _ ->
--     "Grid of Multiple Choices"
-- Db.Matrix Db.Box _ _ ->
--     "Grid of Boxes"
-- HELPERS


type alias RelatedData =
    { id : String
    , name : String
    , study : ( String, Maybe Db.Study )
    , questions : List (OrderAware Db.Question)
    , created : Posix
    , creator : ( String, Maybe Db.User )
    , updated : Posix
    , max_index : Maybe Int
    }


relatedData : String -> Db.Database -> Maybe RelatedData
relatedData id db =
    case Dict.get id db.questionnaries of
        Just timestampedQuestionary ->
            let
                questions = List.sortBy (\( _, y ) -> y.index) <|
                            List.filter (\( _, y ) -> y.questionary == id) <|
                                List.map (\( x, y ) -> ( x, y.value )) <|
                                    Dict.toList db.questions
                questionary =
                    timestampedQuestionary.value
            in
            Just
                { id = id
                , name = questionary.name
                , study = ( questionary.study, Maybe.map .value <| Dict.get questionary.study db.studies )
                , max_index = List.maximum <| List.map (\(_, x)-> (x.index)) questions
                , questions = orderAwareList questions
                , created = Time.millisToPosix timestampedQuestionary.created
                , creator = ( timestampedQuestionary.creator, Maybe.map .value <| Dict.get timestampedQuestionary.creator db.users )
                , updated = Time.millisToPosix timestampedQuestionary.modified
                }

        Nothing ->
            Nothing


type alias OrderAware a =
    { value : a
    , previous : Maybe String
    , next : Maybe String
    , id : String
    }


prePost : Maybe a -> List a -> List ( Maybe a, a, Maybe a )
prePost prev xs =
    case xs of
        [] ->
            []

        a :: [] ->
            [ ( prev, a, Nothing ) ]

        a :: b :: c ->
            ( prev, a, Just b ) :: prePost (Just a) (b :: c)


orderAwareList : List ( String, a ) -> List (OrderAware a)
orderAwareList old =
    let
        mapToValue a =
            case a of
                Just ( id, val ) ->
                    Just id

                Nothing ->
                    Nothing
    in
    prePost Nothing old
        |> List.map (\( x, ( id, value ), y ) -> { value = value, id = id, previous = mapToValue x, next = mapToValue y })


viewStudy : ( String, Maybe Db.Study ) -> Maybe String -> String
viewStudy ( id, mbStudy ) cur =
    Maybe.map .name mbStudy
        |> Maybe.withDefault id


viewList : List ( String, a ) -> (String -> msg) -> Html msg
viewList elements onClick =
    if List.length elements > 0 then
        list { listConfig | nonInteractive = True } <|
            List.map (\( x, _ ) -> listItem { listItemConfig | onClick = Just (onClick x) } [ listItemGraphic [] [ identicon "100%" x ], text x ]) elements

    else
        list listConfig
            [ listItem listItemConfig [ text "Nothing here, create one?" ]
            ]


toTitle : Model -> String
toTitle _ =
    "Home ⧽ Questionary"


textForm : Maybe String -> Form.FormFunctor msg
textForm label value callback =
    TextField.textField
        { textFieldConfig
            | value = value
            , onInput = Just callback
            , label = label
            , outlined = True
        }


wideTextForm : Maybe String -> Form.FormFunctor msg
wideTextForm label value callback =
    TextField.textField
        { textFieldConfig
            | value = value
            , onInput = Just callback
            , label = label
            , fullwidth = True

            -- , outlined = True
        }
