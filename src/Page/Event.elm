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
import Material.DataTable as DataTable
import Material.TabBar as TabBar
import Material.Tab as Tab
import Material.Icon as Icon
import Type.Database as Db
import Material.List as MList exposing (list)
import Material.List.Item as MLItem exposing (listItem, graphic)
import Type.Database.TypeMatching as Match
import Type.IO.Setter as Updater
import Type.IO.Internal as Id exposing (Id, box, unbox)
import Viewer.EditableText as EditableText
import Url.Builder
import Url.Parser as Parser exposing ((</>))
import Url.Parser.Query as Query

{-
   This is a page with subpages. You can change the behaviour depending on the subpage path!
-}
-- MODEL


type alias Model =
    { page : Msg.EventSubPage
    , id : Id Db.Event String
    , nameFocus : Bool
    }

-- INIT


init : Msg.EventSubPage -> Id Db.Event String -> Bool -> Model
init page_ id =
    Model page_ id


page : Session.Session -> Msg.EventSubPage -> Id Db.Event String -> Bool -> ( Page.Page Model Msg.Msg, Cmd Msg.Msg )
page session subpage id focus =
    let
        model =
            { session = session
            , page = init subpage id focus
            , view = view
            , toMsg = identity
            , subscriptions = Sub.none
            -- , header = Viewer.header
            , update = update

            --            , update = Page.liftupdate update
            }
    in
    ( Page model, Cmd.none )


-- parser : Parser.Parser ((String -> Maybe Model) -> a) a
-- parser =
--     Parser.s "answer" </> (Parser.query <|
--         Query.map2 
--             (\qid tsid -> (\eid -> Maybe.map2 Model qid tsid
--                                    |> Maybe.map (\x -> x eid)))
--             (Query.string "qid")
--             (Query.string "tsid"))

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
                Msg.AnswerQuestions {questionary, test_subject, event} ->
                    (Page model, Cmd.none)
                
                Msg.EventSwitchTo _ ->
                    (Page model, Cmd.none)

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
                | title = infos.name
                , user = model.session.user
                , actions = [("settings", Msg.FollowSubpage Db.EventType (unbox infos.id) ["settings"] [])]
                , body =\_ -> 
                    [
                        {- TabBar.tabBar TabBar.config
                            [ Tab.tab
                                (Tab.config
                                    |> Tab.setActive (model.page.page == Msg.EventOverview)
                                    --|> Tab.setOnClick (TabClicked 0)
                                )
                                { label = "Overview", icon = Just <| Tab.icon "poll"}
                            , Tab.tab
                                (Tab.config 
                                    |> Tab.setActive (model.page.page == Msg.EventPeople)
                                    |> Tab.setOnClick (Msg.Event <| Msg.EventSwitchTo <| Msg.EventPeople)
                                )
                                { label = "Participants", icon = Just <| Tab.icon "people" }
                            , Tab.tab
                                (Tab.config 
                                    |> Tab.setActive (model.page.page == Msg.EventSettings)
                                    --|> Tab.setOnClick (TabClicked 1)
                                )
                                { label = "Settings", icon = Just <| Tab.icon "settings" }
                            ]
                         -}
                        case model.page.page of
                            Msg.EventSettings ->
                                text "Settings:"
                            Msg.EventPeople -> 
                                layoutGrid [Typography.typography] [
                                inner [][
                                cell [LG.span8Desktop]
                                    [ Html.h1 [ Typography.headline5 ] [ text "Test Subjects" ]
                                    , viewList infos.test_subjects (Msg.Follow Db.TestSubjectType) (\(x,_) -> String.toUpper <| String.left 4 (unbox x))
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
                                                                    , id = box x
                                                                    , value = unbox infos.id
                                                                    }
                                                            ]
                                            ))
                                        "Add"
                                    ]
                                ]]
                            _ ->
                                layoutGrid [Typography.typography] [
                                inner [][
                                    cell [][
                                    Html.h1 [ Typography.headline5 ] [
                                                EditableText.text 
                                                    econf
                                                    [] 
                                                    (Maybe.withDefault "" <| Maybe.map (\x -> x.value.name) <| Dict.get (unbox model.page.id) db.events)]
                                    , p [][ text <| "Location:" ++ unbox infos.location]
                                    , p [][ unelevated
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
                                                                    , id = box x
                                                                    , value = unbox infos.id
                                                                    }
                                                            ]
                                            ))
                                        "Add Subjects"]
                                    
                                    ]
                                    --, p [][ text <| "Leader: " ++ viewLeader infos.leader model.session.user]   
                                
                                {- , cell []
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
                                        "Add"] -}
                                , cell [][viewTable db infos.questionnaries infos.test_subjects infos.id]                
                                ]]
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
                                    Html.h1 [ Typography.headline5 ] [ text <| "Event not Found: " ++ (unbox model.page.id) ]
                                ]
                            ]
                        ]
                    ]
            }

            
    
    
-- Todo: Add Events, Description, Leader, Name, Questionary

-- HELPERS
type alias RelatedData =
    {
        id : Id Db.Event String,
        name : String,
        location : Id Db.Place String,
        created : Posix,
        creator : (Id Db.User String, Maybe Db.User),
        updated : Posix,
        study : (Id Db.Study String, Maybe Db.Study),
        questionnaries : List (Id Db.Questionary String, Db.Questionary),
        test_subjects : List (Id Db.TestSubject String, Db.TestSubject)
    }

relatedData : Id Db.Event String -> Db.Database -> Maybe RelatedData
relatedData id db =
    case Dict.get (unbox id) db.events of
        Just timestampedEvent ->
            let
                event = timestampedEvent.value
            in
                Just 
                    {
                        id = id,
                        name = event.name,
                        location = event.place,
                        study = (event.study, Maybe.map .value <| Dict.get (unbox event.study) db.studies),
                        created = Time.millisToPosix timestampedEvent.created,
                        creator = (timestampedEvent.creator, Maybe.map .value <| Dict.get (unbox timestampedEvent.creator) db.users),
                        updated = Time.millisToPosix timestampedEvent.modified,
                        questionnaries = List.map (Tuple.mapFirst box) <| Dict.toList <| Dict.map (\x y -> (y.value)) <| Dict.filter (\x y -> y.value.study == event.study) db.questionnaries,
                        test_subjects = List.map (Tuple.mapFirst box) <| Dict.toList <| Dict.map (\x y -> (y.value)) <| Dict.filter (\x y -> y.value.event == id) db.test_subjects
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
viewList : List ( Id a String, a ) -> (String -> msg) -> ((Id a String, a) -> String) -> Html msg
viewList elements onClick nameGetter =
    let
        mlist =  List.map (\( x, y ) -> listItem (MLItem.config |> MLItem.setOnClick (onClick (unbox x)) ) [ graphic [] [ identicon "100%" (unbox x) ], text <| nameGetter (x,y)]) elements
    in
        case mlist of 
            f :: r ->
                list MList.config f r
            _ ->
                list MList.config

                    (listItem MLItem.config [ text "Nothing here, create one?" ])
                    []

viewTable : Db.Database -> List (Id Db.Questionary String, Db.Questionary) -> List (Id Db.TestSubject String, Db.TestSubject) -> Id Db.Event String ->  Html Msg.Msg
viewTable db questionnaries test_subjects event_id =
    DataTable.dataTable DataTable.config
        { thead =
            [ DataTable.row [] <|
                DataTable.cell [] [ text "Subject"] :: List.map (\(x,y) -> DataTable.cell [] [text y.name]) questionnaries
                --[ DataTable.cell [] [ text "Desert" ] ]
            ]
        , tbody =
            List.map (\(test_subject_id,test_subject_value) ->

                    DataTable.row [] <|
                        DataTable.cell [] [text <| String.toUpper <| String.left 4 (unbox test_subject_id)]::List.map (\(questionary_id, questionary_value) ->
                            DataTable.cell [] [ 
                                let
                                    answers = Dict.filter (\answer_id answer_table -> List.member (unbox answer_table.value.question) q_ids ) db.answers
                                              |> Dict.filter (\answer_id answer_table -> answer_table.value.test_subject == test_subject_id)
                                              |> Dict.toList
                                    questions = Dict.filter (\question_id question_table -> question_table.value.questionary == questionary_id) db.questions
                                                |> Dict.toList
                                    q_ids = List.map (\(qid,_)-> qid) questions
                                in
                                    Button.unelevated
                                        (Button.config |> Button.setOnClick (Msg.FollowSubpage Db.EventType (unbox event_id) ["answer"] [Url.Builder.string "qid" (unbox questionary_id), Url.Builder.string "tsid" (unbox test_subject_id)]) ) <| (String.fromInt <| List.length answers) ++ "/" ++ (String.fromInt <| List.length questions)
                            ]
                        )
                        questionnaries
            ) test_subjects
        }
    


toTitle : Model -> String
toTitle model =
    "Home ⧽ Event"
