module Page.Top exposing (Model, init, page, update, view)

import Browser
import Browser.Events
import Browser.Navigation
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events
import Identicon exposing (identicon)
import Json.Decode
import Material.Button as Button exposing (buttonConfig)
import Material.Card as Card exposing (card, cardActionButton, cardActionIcon, cardActions, cardBlock, cardConfig, cardPrimaryActionConfig)
import Material.IconButton exposing (iconButtonConfig)
import Material.LayoutGrid as LG exposing (layoutGrid, layoutGridCell)
import Material.List as ListItem exposing (listConfig, listItemConfig)
import Material.TabBar as TabBar exposing (tabBarConfig, tabConfig)
import Material.Theme as Theme
import Material.Typography as Typography
import Msg
import Page
import Ports
import Session
import Type.Database as Db
import Type.Database.TypeMatching as Match
import Type.IO.Setter as Updater
import Url.Builder
import Utils exposing (..)
import Viewer exposing (detailsConfig)
import Viewer.Internal as I exposing (defaultCardConfig)



{-
   This is the top page - the page that is displayed when the url path is "/"
-}
-- MODEL


type alias Model =
    { localStorageInputField : String
    }



-- INIT


init : Model
init =
    Model ""


page : Session.Session -> ( Page.Page Model Msg.Msg, Cmd Msg.Msg )
page session =
    let
        model =
            { session = session
            , page = init
            , view = view
            , toMsg = identity
            , -- header = Viewer.header,
              update = update
            }
    in
    ( Page.Page model, Cmd.none )



-- UPDATE


update : Msg.Msg -> Page.Page Model Msg.Msg -> ( Page.Page Model Msg.Msg, Cmd Msg.Msg )
update message (Page.Page model) =
    let
        session =
            model.session
    in
    case message of
        Msg.Top msg ->
            case msg of
                Msg.NoOp ->
                    ( Page.Page model, Cmd.none )

                -- Updates the value of the localStorage input field in the model
                Msg.LocalStorageInputFieldChange input ->
                    let
                        oldPage =
                            model.page

                        newPage =
                            { oldPage | localStorageInputField = input }
                    in
                    ( Page.Page { model | page = newPage }, Cmd.none )

                -- Sets the value in local storage (from Set localStorage button, onEnter listener of the localStorage input field)
                Msg.SetLocalStorage ->
                    let
                        localStorage =
                            { token = model.page.localStorageInputField }

                        newSession =
                            session

                        --     { session | localStorage = Just localStorage }
                        oldPage =
                            model.page

                        newPage =
                            { oldPage | localStorageInputField = "" }
                    in
                    ( Page.Page { model | page = newPage, session = newSession }, Ports.toLocalStorage localStorage )

                -- Clears localStorage (from Clear localStorage button)
                Msg.ClearLocalStorage ->
                    let
                        newSession =
                            session

                        --                    { session | localStorage = Nothing }
                    in
                    ( Page.Page { model | session = newSession }, Ports.clearLocalStorage () )

                Msg.SetUser id ->
                    let
                        newSession =
                            { session | user = Just id }
                    in
                    ( Page.Page { model | session = newSession }, Cmd.none )

        -- Msg.ChooseTab tab ->
        --     let
        --         oldPage =
        --             model.page
        --         newPage =
        --             { oldPage | currentTab = tab }
        --     in
        --     ( Page.Page { model | page = newPage }, Cmd.none )
        -- let
        --     newsession = {session | user = Just id}
        -- in
        --     ( Page.Page {model|session = newsession }, Cmd.none)
        _ ->
            ( Page.Page model, Cmd.none )



-- VIEW


view : Page.Page Model Msg.Msg -> Viewer.Details Msg.Msg
view (Page.Page model) =
    let
        db =
            model.session.db
    in
    { detailsConfig
        | title = toTitle
        , body =
            case model.session.user of
                Just user ->
                    let
                        studies =
                            studyOverview user (Page.Page model)
                    in
                    if List.length studies > 0 then
                        [ layoutGrid [] <|
                            List.map (\x -> layoutGridCell [] [ x ]) studies
                        ]

                    else
                        [ text "Create"
                        , Button.textButton
                            { buttonConfig
                                | onClick =
                                    Just
                                        (Msg.CRUD
                                            (Msg.CreateRandom Db.StudyType
                                                [ \x ->
                                                    Match.setField
                                                        { kind = Db.StudyType
                                                        , attribute = "leader"
                                                        , setter = Updater.StringMsg
                                                        , id = x
                                                        , value = user
                                                        }
                                                , Msg.Follow Db.StudyType
                                                ]
                                            )
                                        )
                            }
                            "Create"
                        ]

                -- [tabbedView model.page]
                -- let
                --     relevant_codings = Db.database
                -- in
                -- [
                -- layoutGrid []
                -- [ Html.h2 [Typography.headline6] [text "Please choose your Coding:"]
                --     , layoutGridCell [ LG.span4 ] [ viewCodingCard user model.session.db ] ]
                -- ]
                --[ layoutGridCell [][]
                --]
                Nothing ->
                    [ layoutGrid [] <|
                        selectUser <|
                            Match.keys Db.UserType model.session.db
                    ]
        , user = model.session.user
    }


studyOverview : String -> Page.Page Model Msg.Msg -> List (Html Msg.Msg)
studyOverview user (Page.Page model) =
    let
        db =
            model.session.db
    in
    db.studies
        |> Dict.toList
        |> List.map (\( x, y ) -> ( x, y.value ))
        |> List.filter (\( x, y ) -> y.leader == user)
        --        |> List.map (\(x,y) -> (x, Db.study.viewer db y))
        |> List.map (\( x, y ) -> studyCard x y)


studyCard : String -> Db.Study -> Html Msg.Msg
studyCard id study =
    I.viewCard
        { defaultCardConfig
            | id = id
            , primaryAction = Just <| Msg.Follow Db.StudyType id
        }



-- tabbedView : Model -> Html Msg.Msg
-- tabbedView model =
--     TabBar.tabBar tabBarConfig
--         [ TabBar.tab
--             { tabConfig
--                 | active = model.currentTab == Msg.Questionary
--                 , onClick = Just (Msg.Top <| Msg.ChooseTab Msg.Questionary)
--             }
--             { label = "Questionary", icon = Nothing }
--         , TabBar.tab
--             { tabConfig
--                 | active = model.currentTab == Msg.Coding
--                 , onClick = Just (Msg.Top <| Msg.ChooseTab Msg.Coding)
--             }
--             { label = "Coding", icon = Nothing }
--         ]


selectUser : List String -> List (Html Msg.Msg)
selectUser users =
    if List.length users > 0 then
        if List.length users == 1 then
            [ List.head users
                |> Maybe.withDefault ""
                |> (\x -> layoutGridCell [] [ text <| "i have a user: " ++ x ])
            ]

        else
            [ Html.h2 [ Typography.headline6 ] [ text "Please choose your account:" ]
            , layoutGridCell []
                [ ListItem.list listConfig <|
                    List.map (\user -> ListItem.listItem { listItemConfig | onClick = Just (Msg.Top (Msg.SetUser user)) } [ ListItem.listItemGraphic [] [ identicon "100%" user ], text user ]) users
                ]
            ]

    else
        [ layoutGridCell []
            [ p []
                [ text "Looks like this is the first time you're using msquaredc!"
                ]
            , Button.textButton
                { buttonConfig | onClick = Just (Msg.CRUD (Msg.CreateRandom Db.UserType [])) }
                "Let's go!"
            ]
        ]


viewCodingCard : String -> Db.Database -> Html Msg.Msg
viewCodingCard user db =
    card cardConfig
        { blocks =
            Card.cardPrimaryAction cardPrimaryActionConfig
                [ cardBlock <|
                    div
                        [ Html.Attributes.style "margin-left" "auto"
                        , Html.Attributes.style "margin-right" "auto"
                        , Html.Attributes.style "padding-top" "1rem"
                        , Html.Attributes.style "width" "25%"
                        ]
                        [ identicon "100%" user ]
                , cardBlock <|
                    Html.div [ Html.Attributes.style "padding" "1rem" ]
                        [ Html.h2
                            [ Typography.headline6
                            , Html.Attributes.style "margin" "0"
                            ]
                            [ text <| "Coding: " ++ user ]
                        , Html.h3
                            [ Typography.subtitle2
                            , Theme.textSecondaryOnBackground
                            , Html.Attributes.style "margin" "0"
                            ]
                            [ text "Some interesting Subtitle" ]
                        ]
                , cardBlock <|
                    Html.div
                        [ Html.Attributes.style "padding" "0 1rem 0.5rem 1rem"
                        , Typography.body2
                        , Theme.textSecondaryOnBackground
                        ]
                        [ Html.p [] [ text "Description" ] ]
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



-- HELPERS


toTitle =
    "Home"


highlights =
    ul []
        [ li [] [ text "Client-side routing that uses pushState navigation and the forward slash `/` as the path separator." ]
        , li [] [ text "Search Engine Optimization (SEO) friendly - unique Title for each page." ]
        , li [] [ text "Support for localStorage, with the necessary ports and JS handlers already initalized." ]
        , li [] [ text "Support for responsive site design by listening for window size changes and always storing window size in the model." ]
        , li [] [ text "Built with webpack." ]
        , li [] [ text "Well-commented code!" ]
        ]



-- Custom event listener for the 'Enter' key being pressed


onEnter : Msg.TopMsg -> Attribute Msg.TopMsg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.Decode.succeed msg

            else
                Json.Decode.fail "not ENTER"
    in
    Html.Events.on "keydown" (Json.Decode.andThen isEnter Html.Events.keyCode)