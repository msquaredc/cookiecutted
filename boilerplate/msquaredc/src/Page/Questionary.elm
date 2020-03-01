module Page.Questionary exposing (Model, init, page, update, view)

--import Browser

import Dict
import Html exposing (Html, div, p, text)
import Html.Attributes
import Identicon exposing (identicon)
import Material.Button exposing (buttonConfig)
import Material.Card as Card exposing (cardActionButton, cardActionIcon, cardActions, cardBlock, cardConfig, cardPrimaryActionConfig)
import Material.IconButton exposing (iconButtonConfig)
import Material.LayoutGrid as LG exposing (layoutGrid, layoutGridCell, layoutGridInner)
import Material.List exposing (list, listConfig, listItem, listItemConfig, listItemGraphic)
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
    , activeQuestion : Maybe String
    , titleFocused : Bool
    }



-- INIT


init : String -> Maybe String -> Model
init a b =
    Model a b False


page : Session.Session -> String -> Maybe String -> ( Page.Page Model Msg.Msg, Cmd Msg.Msg )
page session id activeQuestion =
    let
        model =
            { session = session
            , page = init id activeQuestion
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
                        newModel =
                            { oldmodel | activeQuestion = selection }
                    in
                    ( Page { model | page = newModel }, Cmd.none )

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
                , body =
                    [ layoutGrid [ Typography.typography ]
                        [ layoutGridInner [] <|
                            [ layoutGridCell [ LG.span12 ]
                                [ Html.h1 [ Typography.headline5 ] [ text <| "Questionary: " ++ infos.name ]

                                -- , p [][ text <| "Location:" ++ infos.location]
                                , p [] [ text <| "Study: " ++ viewStudy infos.study model.session.user ]
                                ]
                            ]
                                ++ List.map (\x -> layoutGridCell [ LG.span8, LG.alignMiddle ] [ viewQuestionCard db model.page.activeQuestion x ]) infos.questions
                                ++ [ layoutGridCell [ LG.span8, LG.alignBottom ]
                                        [ list listConfig <|
                                            [ listItem
                                                { listItemConfig
                                                    | onClick =
                                                        Just <|
                                                            Msg.CRUD <|
                                                                Msg.CreateRandom Db.QuestionType
                                                                    [ Match.setField Db.QuestionType "questionary" Updater.StringMsg infos.id
                                                                    ]
                                                }
                                                [ Html.h3 [ Typography.headline3, Html.Attributes.style "text-align" "center" ] [ text "+" ] ]
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
                , body =
                    [ layoutGrid []
                        [ layoutGridInner []
                            [ layoutGridCell []
                                [ Html.h1 [ Typography.headline5 ] [ text <| "Questionary not Found" ]
                                ]
                            ]
                        ]
                    ]
            }


viewQuestionCard : Db.Database -> Maybe String -> ( String, Db.Question ) -> Html Msg.Msg
viewQuestionCard db mbCur ( id, question ) =
    if mbCur == Just id then
        Card.card cardConfig
            { blocks =
                [ cardBlock <|
                    Html.div [ Html.Attributes.style "padding" "1rem" ]
                        [ Result.withDefault (div [] []) <| Match.forms id Db.QuestionType "text" db <| wideTextForm Nothing ]
                , cardBlock <|
                    Html.div [ Html.Attributes.style "padding" "1rem" ]
                        [ text <| question.input_type
                            ,Select.outlinedSelect
                            { selectConfig
                                | label = "Question Type"
                                , value = Just (question.input_type)
                                , onChange = Just <| \x -> Match.setField Db.QuestionType "input_type" Updater.StringMsg x id
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
                        [ Maybe.withDefault (div [][] ) <| Maybe.map viewInputType <| IT.fromString question.input_type ]
                ]
            , actions =
                Just <|
                    cardActions
                        { buttons =
                            [ cardActionButton buttonConfig
                                "Visit"
                            ]
                        , icons =
                            [ cardActionIcon iconButtonConfig
                                "favorite"
                            ]
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
                        div [ Html.Attributes.style "padding" "1rem" ]
                            [ text question.text ]
                    , cardBlock <|
                        Html.div [ Html.Attributes.style "padding" "1rem" ]
                            [ Html.p [] [ text "Lorem ipsum…" ] ]
                    ]
            , actions =
                Just <|
                    cardActions
                        { buttons =
                            [ cardActionButton buttonConfig
                                "Visit"
                            ]
                        , icons =
                            [ cardActionIcon iconButtonConfig
                                "favorite"
                            ]
                        }
            }


viewInputType : IT.InputType -> Html msg
viewInputType kind =
    case kind of
        IT.ShortAnswer ->
            TextField.textField
                { textFieldConfig
                    | value = ""
                    , onInput = Nothing
                    , label = Nothing
                    , outlined = True
                }

        IT.LongAnswer ->
            TextArea.textArea
                { textAreaConfig
                    | label = Nothing
                    , value = ""
                    , onInput = Nothing
                    , rows = Just 4
                    , cols = Just 20
                }

        _ ->
            div [] []



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
    , questions : List ( String, Db.Question )
    , created : Posix
    , creator : ( String, Maybe Db.User )
    , updated : Posix
    }


relatedData : String -> Db.Database -> Maybe RelatedData
relatedData id db =
    case Dict.get id db.questionnaries of
        Just timestampedQuestionary ->
            let
                questionary =
                    timestampedQuestionary.value
            in
            Just
                { id = id
                , name = questionary.name
                , study = ( questionary.study, Maybe.map .value <| Dict.get questionary.study db.studies )
                , questions = List.filter (\( _, y ) -> y.questionary == id) <| List.map (\( x, y ) -> ( x, y.value )) <| Dict.toList db.questions
                , created = Time.millisToPosix timestampedQuestionary.created
                , creator = ( timestampedQuestionary.creator, Maybe.map .value <| Dict.get timestampedQuestionary.creator db.users )
                , updated = Time.millisToPosix timestampedQuestionary.modified
                }

        Nothing ->
            Nothing


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
