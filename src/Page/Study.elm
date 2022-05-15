module Page.Study exposing (Model, page)

--import Browser

import Dict
import File.Download as Download
import Html exposing (Html, p, text)
import Identicon exposing (identicon)
import Material.Button as Button exposing (unelevated)
import Material.LayoutGrid exposing (cell, inner, layoutGrid)
import Material.List as MList exposing (list)
import Material.List.Item as MLItem exposing (graphic, listItem)
import Material.Typography as Typography
import Msg
import Page exposing (Page(..))
import Session
import Time exposing (Posix)
import Type.Database as Db
import Type.Database.Aquisition exposing (..)
import Type.Database.TypeMatching as Match
import Type.IO.Internal exposing (Id, box, unbox)
import Type.IO.Setter as Updater
import Viewer exposing (detailsConfig)
import Viewer.EditableText as EditableText



{-
   This is a page with subpages. You can change the behaviour depending on the subpage path!
-}
-- MODEL


type alias Model =
    { id : Id Db.Study String
    , nameFocus : Bool
    }



-- INIT
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


page : Session.Session -> Id Db.Study String -> Bool -> ( Page.Page Model Msg.Msg, Cmd Msg.Msg )
page session id focus =
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
                                old_page =
                                    model.page

                                new_page =
                                    { old_page | nameFocus = True }
                            in
                            ( Page { model | page = new_page }, Cmd.none )

                        Msg.LooseFocus ->
                            let
                                old_page =
                                    model.page

                                new_page =
                                    { old_page | nameFocus = False }
                            in
                            ( Page { model | page = new_page }, Cmd.none )

                Msg.ExportStudy id ->
                    ( Page model, Download.string "export.csv" "text/csv" <| exportStudy (box id) model.session.db )

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
            , deactivator = \_ -> Msg.Study <| Msg.StudyNameEdit Msg.LooseFocus
            , callback =
                \z ->
                    Match.setField
                        { kind = Db.StudyType
                        , attribute = "name"
                        , setter = Updater.StringMsg
                        , id = model.page.id
                        , value = z
                        }
            }
    in
    case mbInfos of
        Just infos ->
            { detailsConfig
                | title = toTitle model.page
                , actions = [ ( "get_app", Msg.Study <| Msg.ExportStudy <| unbox infos.id ) ]
                , user = model.session.user
                , body =
                    \_ ->
                        [ layoutGrid [ Typography.typography ]
                            [ inner []
                                [ cell []
                                    [ Html.h1 [ Typography.headline5 ]
                                        [ {- text "Study: ", -}
                                          EditableText.text
                                            econf
                                            []
                                            infos.title
                                        ]
                                    , p [] [ text <| "Description:" ++ infos.description ]
                                    , p [] [ text <| "Leader: " ++ viewLeader infos.leader model.session.user ]
                                    , p []
                                        [ unelevated
                                            (Button.config
                                                |> Button.setIcon (Just <| Button.icon "add")
                                                |> Button.setOnClick (Msg.FollowSubpage Db.StudyType (unbox model.page.id) [ "code" ] [])
                                             --     --Just <|
                                             -- )
                                            )
                                            "StartCoding"
                                        ]
                                    ]
                                , cell []
                                    [ Html.h1 [ Typography.headline5 ] [ text "Events" ]
                                    , viewList infos.events (Msg.Follow Db.EventType) .name
                                    , unelevated
                                        (Button.config
                                            |> Button.setIcon (Just <| Button.icon "add")
                                            |> Button.setOnClick
                                                --Just <|
                                                (Msg.CRUD <|
                                                    Msg.CreateRandom Db.EventType
                                                        [ \x ->
                                                            Match.setField
                                                                { kind = Db.EventType
                                                                , attribute = "study"
                                                                , setter = Updater.StringMsg
                                                                , id = box x
                                                                , value = unbox infos.id
                                                                }
                                                        ]
                                                )
                                        )
                                        "Add"
                                    ]
                                , cell []
                                    [ Html.h1 [ Typography.headline5 ] [ text "Questionnaries" ]
                                    , viewList infos.questionnaries (Msg.Follow Db.QuestionaryType) .name
                                    , unelevated
                                        (Button.config
                                            |> Button.setIcon (Just <| Button.icon "add")
                                            |> Button.setOnClick
                                                --Just <|
                                                (Msg.CRUD <|
                                                    Msg.CreateRandom Db.QuestionaryType
                                                        [ \x ->
                                                            Match.setField
                                                                { kind = Db.QuestionaryType
                                                                , attribute = "study"
                                                                , setter = Updater.StringMsg
                                                                , id = box x
                                                                , value = unbox infos.id
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
                , body =
                    \_ ->
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
    { id : Id Db.Study String
    , title : String
    , leader : ( Id Db.User String, Maybe Db.User )
    , description : String
    , events : List ( Id Db.Event String, Db.Event )
    , questionnaries : List ( Id Db.Questionary String, Db.Questionary )
    , created : Posix
    , creator : ( Id Db.User String, Maybe Db.User )
    , updated : Posix
    }


relatedData : Id Db.Study String -> Db.Database -> Maybe RelatedData
relatedData id db =
    case Dict.get (unbox id) db.studies of
        Just timestampedStudy ->
            let
                study =
                    timestampedStudy.value
            in
            Just
                { id = id
                , title = study.name
                , leader = ( study.leader, Maybe.map .value <| Dict.get (unbox study.leader) db.users )
                , description = study.description
                , events = List.filter (\( _, y ) -> y.study == id) <| List.map (\( x, y ) -> ( box x, y.value )) <| Dict.toList db.events
                , questionnaries = List.filter (\( _, y ) -> y.study == id) <| List.map (\( x, y ) -> ( box x, y.value )) <| Dict.toList db.questionnaries
                , created = Time.millisToPosix timestampedStudy.created
                , creator = ( timestampedStudy.creator, Maybe.map .value <| Dict.get (unbox timestampedStudy.creator) db.users )
                , updated = Time.millisToPosix timestampedStudy.modified
                }

        Nothing ->
            Nothing


viewLeader : ( Id Db.User String, Maybe Db.User ) -> Maybe (Id Db.User String) -> String
viewLeader ( id, mbLeader ) cur =
    if Just id == cur then
        "You"

    else
        Maybe.andThen .name mbLeader
            |> Maybe.withDefault (unbox id)


viewList : List ( Id a String, a ) -> (String -> msg) -> (a -> String) -> Html msg
viewList elements onClick nameGetter =
    let
        mlist =
            List.map (\( x, y ) -> listItem (MLItem.config |> MLItem.setOnClick (onClick (unbox x))) [ graphic [] [ identicon "100%" (unbox x) ], text <| nameGetter y ]) elements
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


type alias SerializableStudyDatapoint =
    { event : String
    , question : String
    , answer : String
    , coding_question : String
    , coding_answer : String
    -- , coder : String
    }


exportStudy : Id Db.Study String -> Db.Database -> String
exportStudy id db =
    let
        datapoints =
            Aquisition SerializableStudyDatapoint id
                |> start (Value .study) db.events (Value .name)
                |> move (Value .study) db.questionnaries (Raw Tuple.first)
                |> add (Value .questionary) db.questions (Value .text)
                -- -- |> addAttrList (Value .questionary) .questions (Value .input_ty) db (\_ -> ["Implement Me!"])
                |> move (Value .questionary) db.questions (Raw Tuple.first)
                |> add (Value .question) db.answers (Value .value)
                -- --|> moveReferenceList (Value .question) .answers (Raw Tuple.first) db
                |> move (Value .question) db.coding_questionnaries (Raw Tuple.first)
                |> add (Value .coding_questionary) db.coding_questions (Value .text)
                |> move (Value .coding_questionary) db.coding_questions (Raw Tuple.first)
                |> add (Value .coding_question) db.coding_answers (Value .value)
                |> move (Value .coding_question) db.coding_answers (Raw Tuple.first)
                |> move (Raw Tuple.first) db.coding_answers (Raw (\( _, y ) -> y.creator))
                -- |> add (Raw (\( _, y ) -> y.creator)) db.users (Value (\x -> Maybe.withDefault "" x.name))
                |> end
    in
    List.map serializeStudyDatapoint datapoints
        |> String.join "\n"
        |> \x -> "event;question;answer;coding_question;coding_answer\n" ++ x 

serializeStudyDatapoint : SerializableStudyDatapoint -> String
serializeStudyDatapoint data =

    String.join ";"
        [ 
        data.event 
        --, data.coder
        , data.question
        , data.answer 
        , data.coding_question 
        , data.coding_answer 
        ]
