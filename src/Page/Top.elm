module Page.Top exposing (Model, page)

import Dict
import Html exposing (..)
import Material.Button as Button
import Material.LayoutGrid as LG exposing (cell, layoutGrid)
import Msg
import Page
import Ports
import Session
import Type.Database as Db
import Type.Database.TypeMatching as Match
import Type.IO.Internal exposing (Id, box, unbox)
import Type.IO.Setter as Updater
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
              subscriptions = Sub.none
            , update = update
            }
    in
    ( Page.Page model, Cmd.none )



-- UPDATE


update : Msg.Msg -> Page.Page Model Msg.Msg -> ( Page.Page Model Msg.Msg, Cmd Msg.Msg )
update message (Page.Page model) =
    case message of
        Msg.Top msg ->
            let
                session =
                    model.session
            in
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
    { detailsConfig
        | title = toTitle
        , top = True
        , body =
            \_ ->
                case model.session.user of
                    Just user ->
                        let
                            studies =
                                studyOverview user (Page.Page model)
                        in
                        if List.length studies > 0 then
                            [ layoutGrid [] <|
                                [ LG.inner [] <|
                                    List.map (\x -> cell [] [ x ]) studies
                                ]
                            ]

                        else
                            [ text "Create"
                            , Button.text
                                (Button.config
                                    |> Button.setOnClick
                                        (Msg.CRUD
                                            (Msg.CreateRandom Db.StudyType
                                                [ \x ->
                                                    Match.setField
                                                        { kind = Db.StudyType
                                                        , attribute = "leader"
                                                        , setter = Updater.StringMsg
                                                        , id = box x
                                                        , value = unbox user
                                                        }
                                                , Msg.Follow Db.StudyType
                                                ]
                                            )
                                        )
                                )
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
                            [ text "ye should not get here" ]
                        ]
        , user = model.session.user
    }


studyOverview : Id Db.User String -> Page.Page Model Msg.Msg -> List (Html Msg.Msg)
studyOverview user (Page.Page model) =
    let
        db =
            model.session.db
    in
    db.studies
        |> Dict.toList
        |> List.map (\( x, y ) -> ( x, y.value ))
        |> List.filter (\( _, y ) -> y.leader == user)
        --        |> List.map (\(x,y) -> (x, Db.study.viewer db y))
        |> List.map (\( x, y ) -> studyCard x y)


studyCard : String -> Db.Study -> Html Msg.Msg
studyCard id study =
    I.viewCard
        { defaultCardConfig
            | id = id
            , primaryAction = Just <| Msg.Follow Db.StudyType id
            , title = study.name
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
-- HELPERS


toTitle =
    "Home"



-- Custom event listener for the 'Enter' key being pressed
