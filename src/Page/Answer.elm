module Page.Answer exposing (..)

import Dict
import Html exposing (text)
import Html.Attributes exposing (style)
import Msg
import List.Extra
import Page exposing (Page(..))
import Session
import Type.Database as Db
import Viewer exposing (detailsConfig)
import Type.Database.InputType as IT exposing (InputType(..))
import Task
import Type.Database.TypeMatching as Match
import Type.IO.Setter as Updater
import Svg.Attributes exposing (x)

type alias Model =
    { questionary : String
    , event : String
    , test_subject : String
    }



-- INIT


page : Session.Session -> Model -> ( Page.Page Model Msg.Msg, Cmd Msg.Msg )
page session init =
    let
        model =
            { session = session
            , page = init
            , view = view
            , toMsg = identity

            -- , header = Viewer.header
            , update = update

            --            , update = Page.liftupdate update
            }
        {- infos = relatedData session.db init
        qids_tasks = 
            List.map toMsg infos.qids_missing
            |> List.map toCmd
            |> Cmd.batch
        toCmd x = Task.perform (always x) (Task.succeed())
        toMsg x = Msg.CRUD
                        (Msg.CreateRandom Db.AnswerType
                            [ \answerid ->
                                Match.setField
                                    { kind = Db.AnswerType
                                    , attribute = "question"
                                    , setter = Updater.StringMsg
                                    , id = answerid
                                    , value = x
                                    }
                            , \answerid ->
                                Match.setField
                                    { kind = Db.AnswerType
                                    , attribute = "event"
                                    , setter = Updater.StringMsg
                                    , id = answerid
                                    , value = init.event
                                    }
                            , \answerid ->
                                Match.setField
                                    { kind = Db.AnswerType
                                    , attribute = "test_subject"
                                    , setter = Updater.StringMsg
                                    , id = answerid
                                    , value = init.test_subject
                                    }
                            ]
                        ) -}
    in
        {- case List.head infos.qids_missing of 
            Nothing ->
                ( Page model, Cmd.none )
            Just v -> 
                ( Page model, v
                                |> toMsg
                                |> toCmd) -}
        (Page model, Cmd.none)


update : Msg.Msg -> Page.Page Model Msg.Msg -> ( Page.Page Model Msg.Msg, Cmd Msg.Msg )
update message (Page model) =
    ( Page model, Cmd.none )


view : Page Model Msg.Msg -> Viewer.Details Msg.Msg
view (Page.Page model) =
    let
        infos = relatedData db model.page
        db = model.session.db
    in
    { detailsConfig
        | title = toTitle model.page
        , user = model.session.user
        , body =
            \_ ->
                case infos.currentQuestionId of
                    Just qid ->
                        case Dict.get qid <| Dict.fromList infos.questions of
                            Just question ->
                                viewQuestion db question infos.currentAnswer
                            Nothing ->
                                [text "Question not found"]
                    Nothing ->
                        [ Html.div demoContent
                            [ 
                                text "Nothing to do!"
                            ]
                        ]
    }

viewQuestion : Db.Database -> Db.Question -> Maybe (String, Db.Answer) -> List (Html.Html Msg.Msg)
viewQuestion db question mbAnswer =
    let
        mbit = Dict.get question.input_type db.input_types
                |> Maybe.map (\x -> x.value)
    in
        case mbit of
            Nothing ->
                [text "Undefined input type"]
            Just (ShortAnswer s)->
                [text "Short Answer"]
            Just (LongAnswer l) ->
                [text "Long Answer"]
            Just (List l) ->
                [text "List Answer"]


type alias RelatedData =
    { questions : List ( String, Db.Question )
    , answers : List (String, Db.Answer)
    , qids_missing : List (String)
    , currentQuestionId : Maybe String
    , currentAnswer : Maybe (String, Db.Answer)
    }


relatedData : Db.Database -> Model -> RelatedData
relatedData db model =
    let
        questions = db.questions
                |> Dict.filter (\id val -> val.value.questionary == model.questionary)
                |> Dict.filter (\id val -> Dict.member val.value.input_type db.input_types)
                |> Dict.toList
                |> List.sortBy (\(id, val) -> val.value.index)
                |> List.map (\(id, val) -> (id, val.value))
                
        qids = List.map (\(x,_)-> x) questions
        answers = List.map (\qid -> Dict.filter (\id val -> val.value.question == qid) db.answers) qids
                  |> List.map (Dict.filter (\id val -> val.value.event == model.event))
                  |> List.map (Dict.filter (\id val -> val.value.test_subject == model.test_subject))
                  |> List.map (Dict.toList)
                  |> List.map (List.sortBy (\(id, val) -> val.modified))
                  |> List.filterMap (List.Extra.last)
                  |> List.sortBy (\(id, val) -> val.created)
                  
        qids_present = List.map (\(id, val) -> val.value.question) answers
        qids_missing = List.filter (\id -> not <| List.member id qids_present) qids
        currentQuestion = case List.head qids_missing of
                            Just qid ->
                                Just qid
                            Nothing ->
                                List.Extra.last qids_present
        currentAnswer = answers
                        |> List.filter (\(id, val) -> Just val.value.question == currentQuestion)
                        |> List.map (\(id, val) -> (id, val.value))
                        |> List.head
    in 
        { questions =
            questions
            
        , answers = answers
                    |> List.map (\(id, val) -> (id, val.value))
        , qids_missing = qids_missing
        , currentQuestionId = currentQuestion
        , currentAnswer = currentAnswer
        }


demoContent : List (Html.Attribute msg)
demoContent =
    [ Html.Attributes.id "demo-content"
    , style "height" "100%"
    , style "-webkit-box-sizing" "border-box"
    , style "box-sizing" "border-box"
    , style "max-width" "100%"
    , style "padding-left" "16px"
    , style "padding-right" "16px"
    , style "padding-bottom" "100px"
    , style "width" "100%"
    , style "overflow" "auto"
    , style "display" "-ms-flexbox"
    , style "display" "flex"
    , style "-ms-flex-direction" "column"
    , style "flex-direction" "column"
    , style "-ms-flex-align" "center"
    , style "align-items" "center"
    , style "-ms-flex-pack" "start"
    , style "justify-content" "flex-start"
    ]


toTitle : Model -> String
toTitle _ =
    "Home ⧽ Answer"
