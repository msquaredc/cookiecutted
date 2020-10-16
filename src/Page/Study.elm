module Page.Study exposing (Model, init, page, update, view)

--import Browser

import Dict exposing (Dict)
import Html exposing (Html, div, p, text)
import Identicon exposing (identicon)
import Material.Button as Button exposing (unelevated)
import Material.LayoutGrid as LG exposing (layoutGrid, cell, inner)
import Material.List as MList exposing (list)
import Material.List.Item as MLItem exposing (listItem, graphic)
import Material.Typography as Typography
import Material.Icon as Icon
import Material.IconButton as IconButton
import Msg
import Page exposing (Page(..))
import Session
import Time exposing (Posix)
import Type.Database as Db
import Type.Database.TypeMatching as Match
import Type.IO.Setter as Updater
import Viewer exposing (detailsConfig)
import Viewer.EditableText as EditableText



{-
   This is a page with subpages. You can change the behaviour depending on the subpage path!
-}
-- MODEL


type alias Model =
    { id : String
    , nameFocus : Bool
    }



-- INIT


init : String -> Bool -> Model
init =
    Model 
     {-
        { active = False
        , activator = Msg.Study <| Msg.StudyNameEdit Msg.GetFocus
        , deactivator = Msg.Study <| Msg.StudyNameEdit Msg.LooseFocus
        , callback = \y -> Match.setField
                                { kind = Db.StudyType
                                , attribute = "name"
                                , setter = Updater.StringMsg
                                , id = model.page.id
                                , value = y
                                }}
    -}

page : Session.Session -> String -> Bool -> ( Page.Page Model Msg.Msg, Cmd Msg.Msg )
page session id focus=
    let
        model =
            { session = session
            , page = Model id focus
            , view = view
            , toMsg = identity
            , subscriptions = Sub.none
            -- , header = Viewer.header
            , update = update

            --            , update = Page.liftupdate update
            }
    in
    ( Page model, Cmd.none )



-- UPDATE


update : Msg.Msg -> Page.Page Model Msg.Msg -> ( Page.Page Model Msg.Msg, Cmd Msg.Msg )
update message (Page model) =
    case message of
        Msg.Study msg ->
            case msg of
                Msg.StudyNameEdit msg_ ->
                    case msg_ of
                        Msg.GetFocus ->
                            let
                                old_page = model.page
                                new_page = {old_page | nameFocus = True}
                            in
                            
                            ( Page {model| page = new_page}, Cmd.none )
                        Msg.LooseFocus ->
                            let
                                old_page = model.page
                                new_page = {old_page | nameFocus = False}
                            in
                            
                            ( Page {model| page = new_page}, Cmd.none )

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

        econf =
            { active = model.page.nameFocus
            , activator = Msg.Study <| Msg.StudyNameEdit Msg.GetFocus
            , deactivator = \_ -> (Msg.Study <| Msg.StudyNameEdit Msg.LooseFocus)
            , callback = \z -> Match.setField
                                    { kind = Db.StudyType
                                    , attribute = "name"
                                    , setter = Updater.StringMsg
                                    , id = model.page.id
                                    , value = z
                                }}
    in
    case mbInfos of
        Just infos ->
            { detailsConfig
                | title = toTitle model.page
                , user = model.session.user
                , body = \_ -> 
                    [ layoutGrid [ Typography.typography ]
                        [ inner[]
                            [ cell []
                                [ Html.h1 [ Typography.headline5 ] [ {-text "Study: ",-} EditableText.text 
                                                                        econf
                                                                        [] infos.title ]
                                , p [] [ text <| "Description:" ++ infos.description ]
                                , p [] [ text <| "Leader: " ++ viewLeader infos.leader model.session.user ]
                                , p [][ unelevated
                                        (Button.config
                                            |> Button.setIcon (Just <| Button.icon "add")
                                            |> Button.setOnClick (Msg.FollowSubpage Db.StudyType model.page.id ["code"][])
                                            --     --Just <|
                                                    
                                            -- )
                                            )
                                        "StartCoding"]
                                ]
                            , cell []
                                [ Html.h1 [ Typography.headline5 ] [ text "Events" ]
                                , viewList infos.events (Msg.Follow Db.EventType) .name
                                , unelevated
                                    (Button.config
                                        |> Button.setIcon (Just <| Button.icon "add")
                                        |> Button.setOnClick (
                                            --Just <|
                                                Msg.CRUD <|
                                                    Msg.CreateRandom Db.EventType
                                                        [ \x ->
                                                            Match.setField
                                                                { kind = Db.EventType
                                                                , attribute = "study"
                                                                , setter = Updater.StringMsg
                                                                , id = x
                                                                , value = infos.id
                                                                }
                                                        ]
                                        ))
                                    "Add"
                                ]
                            , cell []
                                [ Html.h1 [ Typography.headline5 ] [ text "Questionnaries" ]
                                , viewList infos.questionnaries (Msg.Follow Db.QuestionaryType) .name
                                , unelevated
                                    (Button.config 
                                        |> Button.setIcon (Just <| Button.icon "add")
                                        |> Button.setOnClick (
                                            --Just <|
                                                Msg.CRUD <|
                                                    Msg.CreateRandom Db.QuestionaryType
                                                        [ \x ->
                                                            Match.setField
                                                                { kind = Db.QuestionaryType
                                                                , attribute = "study"
                                                                , setter = Updater.StringMsg
                                                                , id = x
                                                                , value = infos.id
                                                                }
                                                        ]
                                        )
                                        )
                                    "Add"
                                ]
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
                        [ inner []
                            [ cell []
                                [ Html.h1 [ Typography.headline5 ] [ text <| "Study not Found" ]
                                ]
                            ]
                        ]
                    ]
            }



-- Todo: Add Events, Description, Leader, Name, Questionary
-- HELPERS


type alias RelatedData =
    { id : String
    , title : String
    , leader : ( String, Maybe Db.User )
    , description : String
    , events : List ( String, Db.Event )
    , questionnaries : List ( String, Db.Questionary )
    , created : Posix
    , creator : ( String, Maybe Db.User )
    , updated : Posix
    }


relatedData : String -> Db.Database -> Maybe RelatedData
relatedData id db =
    case Dict.get id db.studies of
        Just timestampedStudy ->
            let
                study =
                    timestampedStudy.value
            in
            Just
                { id = id
                , title = study.name
                , leader = ( study.leader, Maybe.map .value <| Dict.get study.leader db.users )
                , description = study.description
                , events = List.filter (\( _, y ) -> y.study == id) <| List.map (\( x, y ) -> ( x, y.value )) <| Dict.toList db.events
                , questionnaries = List.filter (\( _, y ) -> y.study == id) <| List.map (\( x, y ) -> ( x, y.value )) <| Dict.toList db.questionnaries
                , created = Time.millisToPosix timestampedStudy.created
                , creator = ( timestampedStudy.creator, Maybe.map .value <| Dict.get timestampedStudy.creator db.users )
                , updated = Time.millisToPosix timestampedStudy.modified
                }

        Nothing ->
            Nothing


viewLeader : ( String, Maybe Db.User ) -> Maybe String -> String
viewLeader ( id, mbLeader ) cur =
    if Just id == cur then
        "You"

    else
        Maybe.andThen .name mbLeader
            |> Maybe.withDefault id


viewList : List ( String, a ) -> (String -> msg) -> (a -> String) -> Html msg
viewList elements onClick nameGetter =
    let
        mlist =  List.map (\( x, y ) -> listItem (MLItem.config |> MLItem.setOnClick (onClick x) ) [ graphic [] [ identicon "100%" x ], text <| nameGetter y ]) elements
    in
        case mlist of 
            f :: r ->
                list MList.config f r
            _ ->
                list MList.config

                    (listItem MLItem.config [ text "Nothing here, create one?" ])
                    []


toTitle : Model -> String
toTitle _ =
    "Home ⧽ Study"
