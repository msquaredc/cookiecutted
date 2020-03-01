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
import Material.LayoutGrid as LG exposing (layoutGrid, layoutGridCell, layoutGridInner)
import Material.Typography as Typography
import Material.Button as Button exposing (unelevatedButton, buttonConfig)
import Type.Database as Db
import Material.List exposing (list,listConfig, listItem, listItemConfig, listItemGraphic)
import Type.Database.TypeMatching as Match
import Type.IO.Setter as Updater

{-
   This is a page with subpages. You can change the behaviour depending on the subpage path!
-}
-- MODEL


type alias Model =
    { id : String
    }

-- INIT


init : String -> Model
init =
    Model


page : Session.Session -> String -> ( Page.Page Model Msg.Msg, Cmd Msg.Msg )
page session id =
    let
        model =
            { session = session
            , page = init id
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
                Msg.EventMsgNothing ->
                    ( Page model, Cmd.none )

        _ ->
            ( Page model, Cmd.none )



-- VIEW


view : Page Model Msg.Msg -> Viewer.Details Msg.Msg
view (Page.Page model) =
    let
        db = model.session.db
        mbInfos = relatedData model.page.id db
    in
    case mbInfos of
        Just infos ->
            { detailsConfig
                | title = toTitle model.page
                , user = model.session.user
            
                , body =
                    [
                        layoutGrid [Typography.typography] [
                            layoutGridInner [][
                                layoutGridCell [][
                                    Html.h1 [ Typography.headline5 ] [ text <| "Event: "++ infos.id ]
                                    , p [][ text <| "Location:" ++ infos.location]
                                    --, p [][ text <| "Leader: " ++ viewLeader infos.leader model.session.user]                                   
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
                , body =
                    [
                        layoutGrid [] [
                            layoutGridInner [][
                                layoutGridCell [][
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
        updated : Posix
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
                        created = Time.millisToPosix timestampedEvent.created,
                        creator = (timestampedEvent.creator, Maybe.map .value <| Dict.get timestampedEvent.creator db.users),
                        updated = Time.millisToPosix timestampedEvent.modified
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

viewList : List (String, a) -> (String -> msg) -> Html msg
viewList elements onClick =
    if List.length elements > 0 then
        list listConfig <|
            List.map (\(x,_) -> listItem {listItemConfig | onClick = Just (onClick x)} [ listItemGraphic [] [ identicon "100%" x],text x ]) elements
    else
        list listConfig 
            [
                listItem listItemConfig [ text "Nothing here, create one?"]
            ]

toTitle : Model -> String
toTitle _ =
    "Home ⧽ Event"
