module Page.Event exposing (Model, init, page, update, view)

--import Browser

import Msg
import Dict exposing (Dict)
import Html exposing (Html, text, div, p)
import Page exposing (Page(..))
import Session
import Identicon exposing (identicon)
import Time exposing (Posix)
import Viewer exposing (detailsConfig)
import Material.LayoutGrid as LG exposing (layoutGrid, cell, inner)
import Material.Typography as Typography
import Material.Button as Button exposing (unelevated)
import Type.Database as Db
import Material.List as MList exposing (list)
import Material.List.Item as MLItem exposing (listItem, graphic)
import Type.Database.TypeMatching as Match
import Type.IO.Setter as Updater
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


page : Session.Session -> String -> Bool -> ( Page.Page Model Msg.Msg, Cmd Msg.Msg )
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
    case message of
        Msg.Event msg ->
            case msg of
                Msg.EventNameEdit msg_ ->
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
        db = model.session.db
        mbInfos = relatedData model.page.id db
        econf =
            { active = model.page.nameFocus
            , activator = Msg.Event <| Msg.EventNameEdit Msg.GetFocus
            , deactivator = \_ -> (Msg.Event <| Msg.EventNameEdit Msg.LooseFocus)
            , callback = \z -> Match.setField
                                    { kind = Db.EventType
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
            
                , body =\_ -> 
                    [
                        layoutGrid [Typography.typography] [
                            inner [][
                                cell [][
                                    Html.h1 [ Typography.headline5 ] [
                                                EditableText.text 
                                                    econf
                                                    [] (Maybe.withDefault "" <| Maybe.map (\x -> x.value.name) <| Dict.get model.page.id db.events)]
                                    , p [][ text <| "Location:" ++ infos.location]
                                    --, p [][ text <| "Leader: " ++ viewLeader infos.leader model.session.user]   
                                , cell []
                                    [ Html.h1 [ Typography.headline5 ] [ text "Test Subjects" ]
                                    , viewList infos.subjects (Msg.Follow Db.TestSubjectType) (\(x,_) -> String.toUpper <| String.left 4 x)
                                    , unelevated
                                        (Button.config
                                            |> Button.setIcon (Just <| Button.icon "add")
                                            |> Button.setOnClick (
                                                --Just <|
                                                    Msg.CRUD <|
                                                        Msg.CreateRandom Db.TestSubjectType
                                                            [ \x ->
                                                                Match.setField
                                                                    { kind = Db.TestSubjectType
                                                                    , attribute = "event"
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
                                    , viewList 
                                        infos.questionnaries 
                                        (Msg.Follow Db.QuestionaryType) 
                                        (\(_,y) -> .name y)
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
                                                                    , value = Tuple.first infos.study
                                                                    }
                                                            ]
                                            )
                                            )
                                        "Add"]                
                                ]
                                -- , layoutGridCell [][
                                --     Html.h1 [ Typography.headline5 ] [ text "Events" ]
                                --     , viewList infos.events (Msg.Follow Db.EventType)
                                --     , unelevatedButton 
                                --         {buttonConfig| icon = Just "add"
                                --                      , onClick = Just <| 
                                --                                  Msg.CRUD <|
                                --                                  Msg.CreateRandom Db.EventType [
                                --                                      Match.setField Db.EventType "event" Updater.StringMsg infos.id
                                --                                  ] } 
                                --         "Add"
                                -- ]
                                -- , layoutGridCell [][
                                --     Html.h1 [ Typography.headline5 ] [ text "Questionnaries" ]
                                --     , viewList infos.questionnaries (Msg.Follow Db.QuestionaryType)
                                --     , unelevatedButton 
                                --         {buttonConfig| icon = Just "add"
                                --                      , onClick = Just <| 
                                --                                  Msg.CRUD <|
                                --                                  Msg.CreateRandom Db.QuestionaryType [
                                --                                      Match.setField Db.QuestionaryType "event" Updater.StringMsg infos.id
                                --                                  ] } 
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
                    [
                        layoutGrid [] [
                            inner [][
                                cell [][
                                    Html.h1 [ Typography.headline5 ] [ text <| "Event not Found" ]
                                ]
                            ]
                        ]
                    ]
            }

            
    
    
-- Todo: Add Events, Description, Leader, Name, Questionary

-- HELPERS
type alias RelatedData =
    {
        id : String,
        location : String,
        created : Posix,
        creator : (String, Maybe Db.User),
        updated : Posix,
        study : (String, Maybe Db.Study),
        questionnaries : List (String, Db.Questionary),
        subjects : List (String, Db.TestSubject)
    }

relatedData : String -> Db.Database -> Maybe RelatedData
relatedData id db =
    case Dict.get id db.events of
        Just timestampedEvent ->
            let
                event = timestampedEvent.value
            in
                Just 
                    {
                        id = id,
                        location = event.place,
                        study = (event.study, Maybe.map .value <| Dict.get event.study db.studies),
                        created = Time.millisToPosix timestampedEvent.created,
                        creator = (timestampedEvent.creator, Maybe.map .value <| Dict.get timestampedEvent.creator db.users),
                        updated = Time.millisToPosix timestampedEvent.modified,
                        questionnaries = Dict.toList <| Dict.map (\x y -> (y.value)) <| Dict.filter (\x y -> y.value.study == event.study) db.questionnaries,
                        subjects = Dict.toList <| Dict.map (\x y -> (y.value)) <| Dict.filter (\x y -> y.value.event == id) db.test_subjects
                    }
    
        Nothing ->
            Nothing

viewLeader : (String, Maybe Db.User) -> Maybe String -> String
viewLeader (id, mbLeader) cur =
    if Just id == cur then
        "You"
    else
        Maybe.andThen .name mbLeader
        |> Maybe.withDefault id

{- viewList : List (String, a) -> (String -> msg) -> Html msg
viewList elements onClick =
    let
        iList = List.map (\(x,_) -> 
                    MLItem.listItem 
                        (MLItem.config |> MLItem.setOnClick (onClick x))
                        [ MLItem.graphic [] [ identicon "100%" x],text x ]) elements
    in
        case iList of
            f :: r ->
                list MList.config f r
            _ ->
                list MList.config 
                    (
                        MLItem.listItem MLItem.config [ text "Nothing here, create one?"]
                    )
                    []
 -}
viewList : List ( String, a ) -> (String -> msg) -> ((String, a) -> String) -> Html msg
viewList elements onClick nameGetter =
    let
        mlist =  List.map (\( x, y ) -> listItem (MLItem.config |> MLItem.setOnClick (onClick x) ) [ graphic [] [ identicon "100%" x ], text <| nameGetter (x,y)]) elements
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
    "Home ⧽ Event"
