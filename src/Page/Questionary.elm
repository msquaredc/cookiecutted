module Page.Questionary exposing (Fokus, Item, Model, defaultFokus, init, page, update, view)

--import Browser

import Dict
import DnDList
import Html exposing (Html, div, p, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Identicon exposing (identicon)
import List.Extra
import Material.Button as Button
import Material.Card as Card exposing (actions, block, primaryAction)
import Material.Checkbox as Checkbox
import Material.Fab as Fab
import Material.Icon as Icon
import Material.IconButton as IconButton
import Material.LayoutGrid as LG exposing (cell, inner, layoutGrid)
import Material.List as MList exposing (list)
import Material.List.Item as MLItem exposing (graphic, listItem)
import Material.Menu as Menu
import Material.Radio as Radio
import Material.Select as Select
import Material.Select.Item as SelectItem
import Material.TextArea as TextArea
import Material.TextField as TextField
import Material.Typography as Typography
import Msg
import Page exposing (Page(..))
import Session
import Svg.Attributes exposing (x)
import Task
import Time exposing (Posix)
import Type.Database as Db
import Type.Database.InputType as IT
import Type.Database.TypeMatching as Match
import Type.IO.Form as Form
import Type.IO.Internal as Id exposing (Id, box, unbox)
import Type.IO.Setter as Updater
import Viewer exposing (detailsConfig, system)
import Viewer.OrderAwareList exposing (OrderAware, orderAwareList)



{-
   This is a page with subpages. You can change the behaviour depending on the subpage path!
-}
-- MODEL


type alias Model =
    { id : Id Db.Questionary String
    , questionary : Maybe (Db.Timestamp Db.Questionary)
    , focus : Fokus
    , dnd : DnDList.Model
    , questions : List Item
    , menu : Maybe String
    }


type alias Item =
    { id : String
    , question : Db.Timestamp Db.Question
    }


type alias Fokus =
    { activeQuestion : Maybe String
    , titleFocused : Bool
    }


defaultFokus : Fokus
defaultFokus =
    Fokus Nothing False



-- INIT


init : Id Db.Questionary String -> Fokus -> DnDList.Model -> List Item -> Maybe (Db.Timestamp Db.Questionary) -> Model
init id focus dnd questions questionary =
    Model id questionary focus dnd questions Nothing


page : Session.Session -> Id Db.Questionary String -> Fokus -> Maybe (List Item) -> DnDList.Model -> ( Page.Page Model Msg.Msg, Cmd Msg.Msg )
page session id focus mbquestions dndmodel =
    let
        model =
            { session = session
            , page = init id focus dndmodel questions questionary
            , view = view
            , toMsg = identity
            , subscriptions = system.subscriptions dndmodel

            -- , header = Viewer.header
            , update = update

            --            , update = Page.liftupdate update
            }

        dbquestions =
            Dict.filter (\qid question -> question.value.questionary == id) session.db.questions
                |> Dict.toList
                |> List.sortBy (\( _, question ) -> question.value.index)
                |> List.map (\( a, b ) -> Item a b)

        questions =
            Maybe.withDefault dbquestions mbquestions

        questionary =
            Dict.get (unbox id) session.db.questionnaries
    in
    ( Page model, Cmd.none )



{- dndSystem : DnDList.System Item Msg.Msg
   dndSystem =
       let
           config = { beforeUpdate = \_ _ list -> list
                   , movement = DnDList.Vertical
                   , listen = DnDList.OnDrag
                   , operation = DnDList.Rotate
                   }
           system = DnDList.create config (Msg.DnDEvent)
       in
           system
-}
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

                Msg.QuestionNameEdit msg_ ->
                    case msg_ of
                        Msg.GetFocus ->
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

                Msg.ContextMenu id ->
                    let
                        newmodel =
                            { oldmodel | menu = id }
                    in
                    ( Page { model | page = newmodel }, Cmd.none )

        {- Msg.OnQuestionDrag msg_ ->
               let
                   ( dnd, items ) =
                       system.update msg_ oldmodel.dnd oldmodel.questions
                   newmodel = { oldmodel | dnd = dnd, questions = items }
               in
               ( Page { model | page = newmodel }
               , Cmd.batch [system.commands dnd {-,changeIndices oldmodel.questions items-}]
               )
           Msg.Tock _ ->
               (Page model, Cmd.none)
        -}
        Msg.DnDEvent msg_ ->
            let
                ( dnd, items ) =
                    system.update msg_ oldmodel.dnd oldmodel.questions

                newmodel =
                    { oldmodel | dnd = dnd, questions = items }
            in
            ( Page { model | page = newmodel }
            , system.commands dnd
            )

        _ ->
            ( Page model, Cmd.none )


changeIndices : List Item -> List Item -> Cmd Msg.Msg
changeIndices old new =
    List.map2
        changeIndex
        old
        new
        |> List.filterMap identity
        |> Match.setManyFields
        |> Task.succeed
        |> Task.perform identity


changeIndex : Item -> Item -> Maybe (Match.FieldConfig Int Db.Question)
changeIndex old new =
    if old.question.value.index == new.question.value.index then
        Nothing

    else
        let
            setIndex id index =
                { kind = Db.QuestionType
                , attribute = "index"
                , setter = Updater.IntMsg
                , id = box id
                , value = index
                }
        in
        Just <| setIndex new.id old.question.value.index



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
            let
                newMsg =
                    Msg.CRUD <|
                        Msg.CreateRandom Db.QuestionType
                            [ \x ->
                                Match.setField
                                    { kind = Db.QuestionType
                                    , attribute = "questionary"
                                    , setter = Updater.StringMsg
                                    , id = box x
                                    , value = unbox infos.id
                                    }
                            , \x ->
                                Match.setField
                                    { kind = Db.QuestionType
                                    , attribute = "index"
                                    , setter = Updater.IntMsg
                                    , id = box x
                                    , value = Maybe.withDefault 0 <| Maybe.map ((+) 1) infos.max_index
                                    }
                            ]
            in
            { detailsConfig
                | title = toTitle model.page
                , user = model.session.user
                , body =
                    \_ ->
                        [ layoutGrid [ Typography.typography ]
                            [ inner [] <|
                                [ cell [ LG.span12 ]
                                    [ Html.h1 [ Typography.headline5 ]
                                        [ editableText
                                            model.page.focus.titleFocused
                                            (Msg.Questionary <| Msg.QuestionNameEdit <| Msg.GetFocus)
                                            (\x -> Msg.Questionary <| Msg.QuestionNameEdit <| Msg.LooseFocus)
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
                                    --++ List.map (\x -> cell [LG.span10Desktop, LG.span8Tablet] [ viewQuestionCard db model.page.focus.activeQuestion x ]) infos.questions
                                    ++ [ cell [ LG.span12 ]
                                            [ inner []
                                                [ cell [ LG.span2Desktop, LG.span1Tablet ] []
                                                , cell [ LG.span8Desktop, LG.span6Tablet ] <|
                                                    [ viewQuestionList model.page db infos infos.questions ]

                                                --viewDraggableQuestionList model.page
                                                ]
                                            ]
                                       ]

                            -- ++ [ cell []
                            --         [ list MList.config
                            --             (listItem
                            --                 (MLItem.config
                            --                     |> MLItem.setOnClick
                            --                         (Msg.CRUD <|
                            --                             Msg.CreateRandom Db.QuestionType
                            --                                 [ \x ->
                            --                                     Match.setField
                            --                                         { kind = Db.QuestionType
                            --                                         , attribute = "questionary"
                            --                                         , setter = Updater.StringMsg
                            --                                         , id = x
                            --                                         , value = infos.id
                            --                                         }
                            --                                 , \x ->
                            --                                     Match.setField
                            --                                         { kind = Db.QuestionType
                            --                                         , attribute = "index"
                            --                                         , setter = Updater.IntMsg
                            --                                         , id = x
                            --                                         , value = Maybe.withDefault 0 <| Maybe.map ((+) 1) infos.max_index
                            --                                         }
                            --                                 ]
                            --                         )
                            --                 )
                            --                 [ Html.h3 [ Typography.headline3, Html.Attributes.style "justify-content" "center" ] [ text "+" ] ]
                            --             )
                            --             []
                            -- ]
                            -- , layoutGridCell [][
                            --     Html.h1 [ Typography.headline5 ] [ text "Questionnaries" ]
                            --     , viewList infos.questionnaries (Msg.Follow Db.QuestionaryType)
                            --     , unelevatedButton
                            --         {buttonConfig| icon = Just "add"
                            --                      , onClick =  }
                            --         "Add"
                            -- ]
                            --    ]
                            ]
                        , Fab.fab
                            (Fab.config
                                |> Fab.setOnClick newMsg
                                |> Fab.setAttributes
                                    [ style "position" "fixed"
                                    , style "bottom" "2rem"
                                    , style "right" "2rem"
                                    ]
                            )
                            (Fab.icon "playlist_add")
                        ]
            }

        Nothing ->
            { detailsConfig
                | title = toTitle model.page
                , user = model.session.user
                , body =
                    \_ ->
                        [ layoutGrid []
                            [ inner []
                                [ cell []
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
        TextField.outlined
            (TextField.config
                |> TextField.setValue (Just value)
                |> TextField.setOnInput callback
                |> TextField.setOnChange deactivator
                |> TextField.setLabel Nothing
                -- , fullwidth = True
                |> TextField.setAttributes [ Typography.headline5 ]
            )
        --     ]
        -- ]

    else
        -- list {listConfig | nonInteractive = True }
        --     [ listItem {listItemConfig | onClick = Just activator}
        --         [
        div [ Typography.headline5, onClick <| activator ] [ text value ]



--     ]
-- ]


viewDraggableQuestionList : Model -> List (Html Msg.Msg)
viewDraggableQuestionList model =
    [ case List.indexedMap (itemView model.dnd) model.questions of
        first :: rest ->
            MList.list MList.config
                first
                rest

        _ ->
            Html.text " "
    , ghostView model.dnd model.questions
    ]



-- let
--     questions = List.indexedMap (itemView model.dnd) model.questions
-- in
--     case questions of
--         first :: rest ->
--             [list
--                 (MList.config
--                     |> MList.setTwoLine True
--                     --|> MList.setNonInteractive True
--                 )
--                 first
--                 rest
--             , ghostView model.dnd model.questions
--             ]
--         _ ->
--             [text "NoItem"]


itemView : DnDList.Model -> Int -> Item -> MLItem.ListItem Msg.Msg
itemView dnd index item =
    let
        itemId : String
        itemId =
            "id-" ++ item.id

        --system = dndSystem
    in
    case system.info dnd of
        Just { dragIndex } ->
            if dragIndex /= index then
                MLItem.listItem
                    (MLItem.config
                        |> MLItem.setAttributes (Html.Attributes.id itemId :: system.dropEvents index itemId)
                    )
                    [ Html.text item.id ]

            else
                MLItem.listItem
                    (MLItem.config
                        |> MLItem.setAttributes [ Html.Attributes.id itemId ]
                    )
                    [ Html.text "[---------]" ]

        Nothing ->
            MLItem.listItem
                (MLItem.config
                    |> MLItem.setAttributes (Html.Attributes.id itemId :: system.dragEvents index itemId)
                )
                [ Html.text item.id ]



{- let
       itemId : String
       itemId =
           "id-" ++ item.id
   in
   case dndSystem.info dnd of
       Just { dragIndex } ->
           if dragIndex /= index then
               listItem
               (MLItem.config
               |> MLItem.setAttributes (Html.Attributes.id itemId :: dndSystem.dropEvents index itemId)
               |> MLItem.setOnClick (Msg.Follow Db.QuestionType item.id)
               )
                   [ MLItem.text [] {primary = [Html.text item.id ], secondary = [] }, MLItem.meta [] [ Icon.icon [] "star" ]]

           else
               listItem
               (MLItem.config |> MLItem.setAttributes  [ Html.Attributes.id itemId ]
               )
                   [ MLItem.text [] {primary =  [ Html.text "[---------]" ], secondary = [] } ]

       Nothing ->
            listItem
               (MLItem.config
               |> MLItem.setAttributes [Html.Attributes.id itemId]
               |> MLItem.setOnClick (Msg.Follow Db.QuestionType item.id)
               )
                   [ MLItem.text [] {primary = [Html.text item.id ], secondary = [] }, MLItem.meta [] [ Icon.icon (dndSystem.dragEvents index itemId) "star" ] ]
-}


ghostView : DnDList.Model -> List Item -> Html.Html Msg.Msg
ghostView dnd items =
    let
        maybeDragItem : Maybe Item
        maybeDragItem =
            system.info dnd
                |> Maybe.andThen (\{ dragIndex } -> items |> List.drop dragIndex |> List.head)
    in
    case maybeDragItem of
        Just item ->
            list
                (MList.config
                    |> MList.setTwoLine True
                    |> MList.setInteractive False
                )
                (listItem
                    (MLItem.config |> MLItem.setAttributes (system.ghostStyles dnd))
                    [ MLItem.text [] { primary = [ Html.text item.id ], secondary = [] } ]
                )
                []

        Nothing ->
            Html.text ""


viewQuestionList : Model -> Db.Database -> RelatedData -> List (OrderAware Db.Question) -> Html Msg.Msg
viewQuestionList model db infos questions =
    case questions of
        first :: rest ->
            list
                (MList.config
                    |> MList.setTwoLine True
                )
                (viewQuestionListItem model db first)
            <|
                List.map (viewQuestionListItem model db) rest

        _ ->
            text "NoItem"


viewQuestionListItem : Model -> Db.Database -> OrderAware Db.Question -> MLItem.ListItem Msg.Msg
viewQuestionListItem model db { id, value, previous, next } =
    let
        x =
            1

        --upMsg = Match.swapFields Db.QuestionType "index" Updater.IntMsg ( prev.id, id ) ( prev.value.index, value.index )
        --downMsg = Match.swapFields Db.QuestionType "index" Updater.IntMsg ( post.id, id ) ( post.value.index, value.index )
    in
    listItem
        (MLItem.config {- |> MLItem.setOnClick (Msg.Follow Db.QuestionType id) -})
    <|
        [ MLItem.text [ onClick <| Msg.Follow Db.QuestionType (unbox id) ]
            { primary = [ Html.text value.text ]
            , secondary = [ Html.text <| Maybe.withDefault (unbox value.input_type) <| Maybe.map (\it -> IT.toString it.value) <| Dict.get (unbox value.input_type) db.input_types ]
            }
        , MLItem.meta []
            [ Html.div [ Menu.surfaceAnchor ]
                [ IconButton.iconButton
                    (IconButton.config
                        |> IconButton.setOnClick
                            (Msg.Questionary <| Msg.ContextMenu <| Just <| unbox id)
                    )
                    (IconButton.icon "more_vert")
                ]
            , Menu.menu
                (Menu.config
                    |> Menu.setOpen (model.menu == Just (unbox id))
                    |> Menu.setOnClose (Msg.Questionary <| Msg.ContextMenu <| Nothing)
                )
                (MLItem.listItem MLItem.config
                    [ text "Menu item" ]
                )
                [ MLItem.listItem MLItem.config
                    [ text "Menu item" ]
                ]
            ]
        ]



{- ++ (case ( previous, next ) of
        ( Just prev, Just post ) ->
            [ MLItem.meta []
                [ IconButton.iconButton
                    (IconButton.config
                        |> IconButton.setOnClick
                            (Match.swapFields Db.QuestionType "index" Updater.IntMsg ( prev.id, id ) ( prev.value.index, value.index ))
                    )
                    (IconButton.icon "arrow_upward")
                , IconButton.iconButton
                    (IconButton.config
                        |> IconButton.setOnClick
                            (Match.swapFields Db.QuestionType "index" Updater.IntMsg ( post.id, id ) ( post.value.index, value.index ))
                    )
                    (IconButton.icon "arrow_downward")
                ]
            ]

        ( Just prev, Nothing ) ->
            [ MLItem.meta []
                [ IconButton.iconButton
                    (IconButton.config
                        |> IconButton.setOnClick
                            (Match.swapFields Db.QuestionType "index" Updater.IntMsg ( prev.id, id ) ( prev.value.index, value.index ))
                    )
                    (IconButton.icon "arrow_upward")
                ]
            ]

        ( Nothing, Just post ) ->
            [ MLItem.meta []
                [ IconButton.iconButton
                    (IconButton.config
                        |> IconButton.setOnClick
                            (Match.swapFields Db.QuestionType "index" Updater.IntMsg ( post.id, id ) ( post.value.index, value.index ))
                    )
                    (IconButton.icon "arrow_downward")
                ]
            ]

        _ ->
            []
   )
-}


viewQuestionCard : Db.Database -> Maybe (Id Db.Question String) -> OrderAware Db.Question -> Html Msg.Msg
viewQuestionCard db mbCur { id, value, previous, next } =
    let
        setMsg x callback =
            Match.setField
                { kind = Db.QuestionType
                , attribute = "input_type"
                , setter = \y -> Updater.Custom y callback
                , value = unbox id
                , id = box x
                }

        question =
            value
    in
    if mbCur == Just id then
        Card.card Card.config
            { blocks =
                [ block <|
                    Html.div [ Html.Attributes.style "padding" "1rem" ]
                        [ Result.withDefault (div [] []) <| Match.forms (unbox id) Db.QuestionType "text" db <| wideTextForm Nothing ]
                , block <|
                    Html.div [ Html.Attributes.style "padding" "1rem" ] <|
                        let
                            mlist =
                                List.map
                                    (\x ->
                                        SelectItem.selectItem
                                            (SelectItem.config { value = IT.toString x })
                                            (IT.toString x)
                                    )
                                    IT.inputTypes
                        in
                        case mlist of
                            f :: r ->
                                [ Select.outlined
                                    (Select.config
                                        |> Select.setLabel (Just "Question Type")
                                        |> Select.setSelected (Just (unbox question.input_type))
                                        |> Select.setOnChange (\x -> setMsg x Nothing)
                                    )
                                    f
                                    r
                                ]

                            _ ->
                                []
                , block <|
                    Html.div [ Html.Attributes.style "padding" "1rem" ]
                        [{- viewInputTypeActive question.input_type <| setMsg (IT.toString question.input_type) -}]
                ]
            , actions =
                Just <|
                    actions
                        { buttons =
                            [ Card.button Button.config
                                "Visit"
                            ]
                        , icons =
                            (case previous of
                                Just prev ->
                                    [ Card.icon (IconButton.config |> IconButton.setOnClick (Match.swapFields Db.QuestionType "index" Updater.IntMsg ( prev.id, id ) ( prev.value.index, value.index )))
                                        (IconButton.icon "arrow_upward")
                                    ]

                                Nothing ->
                                    []
                            )
                                ++ (case next of
                                        Just post ->
                                            [ Card.icon (IconButton.config |> IconButton.setOnClick (Match.swapFields Db.QuestionType "index" Updater.IntMsg ( post.id, id ) ( post.value.index, value.index )))
                                                (IconButton.icon "arrow_downward")
                                            ]

                                        Nothing ->
                                            []
                                   )
                        }
            }

    else
        Card.card Card.config
            { blocks =
                Card.primaryAction
                    [ Html.Events.onClick <| Msg.Questionary <| Msg.CurrentQuestionSelected <| Just (unbox id) ]
                    [ block <|
                        div [ Html.Attributes.style "padding" "1rem", Typography.headline6 ]
                            [ text question.text ]
                    , block <|
                        Html.div [ Html.Attributes.style "padding" "1rem" ]
                            [{- viewInputTypePassive question.input_type -}]
                    ]
            , actions =
                Just <|
                    actions
                        { buttons =
                            [ Card.button Button.config
                                "Visit"
                            ]
                        , icons =
                            (case previous of
                                Just prev ->
                                    [ Card.icon (IconButton.config |> IconButton.setOnClick (Match.swapFields Db.QuestionType "index" Updater.IntMsg ( prev.id, id ) ( prev.value.index, value.index )))
                                        (IconButton.icon "arrow_upward")
                                    ]

                                Nothing ->
                                    []
                            )
                                ++ (case next of
                                        Just post ->
                                            [ Card.icon (IconButton.config |> IconButton.setOnClick (Match.swapFields Db.QuestionType "index" Updater.IntMsg ( post.id, id ) ( post.value.index, value.index )))
                                                (IconButton.icon "arrow_downward")
                                            ]

                                        Nothing ->
                                            []
                                   )
                        }
            }


viewInputTypeActive : IT.InputType -> (Maybe Updater.Msg -> Msg.Msg) -> Html Msg.Msg
viewInputTypeActive kind callback =
    case kind of
        IT.ShortAnswer config ->
            TextField.outlined
                TextField.config

        IT.LongAnswer config ->
            TextArea.filled TextArea.config

        IT.List config ->
            let
                mlist =
                    List.indexedMap
                        (\index x ->
                            listItem MLItem.config
                                [ viewSingleInputType config.singleInput
                                , TextField.outlined
                                    (TextField.config
                                        |> TextField.setValue (Just x)
                                        |> TextField.setOnInput
                                            (\y ->
                                                callback <|
                                                    Just
                                                        (Updater.AttributeMsg "choices" <|
                                                            Updater.ListMixedUpdate index <|
                                                                Updater.StringMsg y
                                                        )
                                            )
                                        |> TextField.setPlaceholder (Just "Add a question")
                                    )
                                ]
                        )
                        (config.choices ++ [ "" ])
            in
            case mlist of
                f :: r ->
                    list (MList.config |> MList.setInteractive False) f r

                _ ->
                    Html.text "No entry"



-- _ ->
--     div [] []


viewInputTypePassive : IT.InputType -> Html msg
viewInputTypePassive kind =
    case kind of
        IT.ShortAnswer config ->
            TextField.outlined TextField.config

        IT.LongAnswer config ->
            TextArea.outlined TextArea.config

        IT.List config ->
            let
                tlist =
                    List.indexedMap
                        (\index x ->
                            listItem MLItem.config
                                [ viewSingleInputType config.singleInput
                                , text x
                                ]
                        )
                        config.choices
            in
            case tlist of
                f :: r ->
                    list (MList.config |> MList.setInteractive False) f r

                _ ->
                    Html.text "List is empty"


viewSingleInputType : IT.SingleInputType -> Html msg
viewSingleInputType kind =
    case kind of
        IT.Box ->
            Checkbox.checkbox Checkbox.config

        IT.Radio ->
            Radio.radio Radio.config



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
    { id : Id Db.Questionary String
    , name : String
    , study : ( Id Db.Study String, Maybe Db.Study )
    , questions : List (OrderAware Db.Question)
    , created : Posix
    , creator : ( Id Db.User String, Maybe Db.User )
    , updated : Posix
    , max_index : Maybe Int
    }


relatedData : Id Db.Questionary String -> Db.Database -> Maybe RelatedData
relatedData id db =
    case Dict.get (unbox id) db.questionnaries of
        Just timestampedQuestionary ->
            let
                questions =
                    List.sortBy (\( _, y ) -> y.index) <|
                        List.filter (\( _, y ) -> y.questionary == id) <|
                            List.map (\( x, y ) -> ( box x, y.value )) <|
                                Dict.toList db.questions

                questionary =
                    timestampedQuestionary.value
            in
            Just
                { id = id
                , name = questionary.name
                , study = ( questionary.study, Maybe.map .value <| Dict.get (unbox questionary.study) db.studies )
                , max_index = List.maximum <| List.map (\( _, x ) -> x.index) questions
                , questions = orderAwareList questions
                , created = Time.millisToPosix timestampedQuestionary.created
                , creator = ( timestampedQuestionary.creator, Maybe.map .value <| Dict.get (unbox timestampedQuestionary.creator) db.users )
                , updated = Time.millisToPosix timestampedQuestionary.modified
                }

        Nothing ->
            Nothing


viewStudy : ( Id Db.Study String, Maybe Db.Study ) -> Maybe (Id Db.User String) -> String
viewStudy ( id, mbStudy ) cur =
    Maybe.map .name mbStudy
        |> Maybe.withDefault (unbox id)


viewList : List ( String, a ) -> (String -> msg) -> Html msg
viewList elements onClick =
    let
        mlist =
            List.map (\( x, _ ) -> listItem (MLItem.config {- |> MLItem.setOnClick (onClick x) -}) [ MLItem.graphic [] [ identicon "100%" x ], text x ]) elements
    in
    case mlist of
        f :: r ->
            list (MList.config |> MList.setInteractive False) f r

        _ ->
            list MList.config
                (listItem MLItem.config [ text "Nothing here, create one?" ])
                []


toTitle : Model -> String
toTitle model =
    Maybe.withDefault "Home ⧽ Questionary" <| Maybe.map (\x -> x.value.name) model.questionary


textForm : Maybe String -> Form.FormFunctor msg
textForm label value callback =
    TextField.outlined
        (TextField.config
            |> TextField.setValue (Just value)
            |> TextField.setOnInput callback
            |> TextField.setLabel label
         --|> TextField.setOutlined
        )


wideTextForm : Maybe String -> Form.FormFunctor msg
wideTextForm label value callback =
    TextField.filled
        (TextField.config
            |> TextField.setValue (Just value)
            |> TextField.setOnInput callback
            |> TextField.setLabel label
         --|> TextField.setFullwidth True
         --|> TextField.setOutlined
        )
