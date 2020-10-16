module Page.Code exposing (..)

import Dict
import Element exposing (fill, height, width, px, padding )
import Element.Font as Font
import Element.Background as Background
import Html exposing (text)
import Html.Attributes exposing (style)
--import Html.Keyed as Keyed
import Element.Keyed as Keyed
import List.Extra
import Material.Button as Button exposing (config)
import Material.TextArea as TextArea
import Material.TextField as TextField
import Msg
import Page exposing (Page(..))
import Session
import Type.Database as Db
import Type.Database.InputType exposing (InputType(..))
import Type.Database.TypeMatching as Match
import Type.IO.Setter as Updater
import Viewer exposing (detailsConfig)
import Url.Parser as Parser exposing ((</>))
import Url.Parser.Query as Query
import Type.Database exposing (Answer)


type alias Model =
    { id : String
    , templates : List (CodingAnswerTemplate)
    , answer : Maybe (String, Db.Timestamp Db.CodingAnswer)
--    , answers : List (String, Db.Timestamp Db.CodingAnswer)
    , current : Maybe (CodingAnswerTemplate)
    , previous : Maybe (Msg.Msg)
    , next : Maybe (Msg.Msg)
    }

type alias CodingAnswerTemplate =
    {
        answerId : String
        , answer : Db.Timestamp Db.Answer
        , questionId : String
        , question : Db.Timestamp Db.Question
        , coding_questionId : String
        , coding_question : Db.Timestamp Db.CodingQuestion
        , input_typeId : String
        , input_type : Db.Timestamp InputType
    }

init : String -> Db.Database -> Model
init id db =
    let
        question2codingQuestionary : (String, Db.Timestamp Db.Question ) -> List (String, Db.Timestamp Db.CodingQuestionary)
        question2codingQuestionary (qid, value ) =
            Dict.filter (\cid cq -> cq.value.question == qid) db.coding_questionnaries
            |> Dict.toList
        codingQuestionary2codingQuestion : (String, Db.Timestamp Db.CodingQuestionary ) -> List (String, Db.Timestamp Db.CodingQuestion)
        codingQuestionary2codingQuestion (qid, value) =
            Dict.filter (\cid cq -> cq.value.coding_questionary == qid) db.coding_questions
            |> Dict.toList
        codingQuestion2input_type (qid, value) =
            Dict.filter (\itid it -> value.value.input_type == itid) db.input_types
            |> Dict.toList
        answers = Dict.filter (\eid event -> event.value.study == id) db.events
                  |> Dict.toList
                  |> List.map (\(eid,event) -> Dict.filter (\aid answer -> answer.value.event == eid) db.answers)
                  |> List.map (Dict.toList)
                  |> List.concat
        all_coding_answers = List.map (\(answer_id,answer) -> ((answer_id, answer),Dict.filter (\question_id question -> answer.value.question == question_id) db.questions )) answers
                         |> List.map (\(answer, questiondict)-> (answer, Dict.toList questiondict))
                         |> List.map (\(answer, questions) -> List.map (\question -> (answer,question)) questions)
                         |> List.concat
                         |> List.map (\(answer, question) -> (answer, question, question2codingQuestionary question ))
                         |> List.map (\(answer, question, codingQuestionnaries) -> List.map (\codingQuestionary -> (answer,question,codingQuestionary)) codingQuestionnaries )
                         |> List.concat
                         |> List.map (\(answer, question, questionary) -> (answer, question, codingQuestionary2codingQuestion questionary ))
                         |> List.map (\(answer, question, codingQuestions) -> List.map (\codingQuestion -> (answer,question,codingQuestion)) codingQuestions )
                         |> List.concat
                         |> List.map (\(a, q, c) -> (a, q, (c,codingQuestion2input_type c )))
                         |> List.map (\(a, q, (c,cl))->List.map (\cs -> (a, q, (c,cs))) cl)
                         |> List.concat
                         |> List.map (\(answer, question, (coding_question, input_type)) -> 
                                {answer = answer
                                , question = question
                                , coding_question = coding_question
                                , input_type = input_type})
        all_relevant_keys : List (String, String)
        all_relevant_keys = List.map (\{answer,coding_question} -> (answer, coding_question)) all_coding_answers
                            |> List.map (\((aid,_),(cqid,_))-> (aid, cqid))
        present_coding_answers : List (String, Db.Timestamp Db.CodingAnswer)
        present_coding_answers = Dict.filter (\cai cav -> List.member (cav.value.answer, cav.value.coding_question) all_relevant_keys) db.coding_answers
                                 |> Dict.toList
                                 |> List.sortBy (\(_,cav) -> cav.accessed)
        history : List (String, Db.Timestamp Db.CodingAnswer)
        history = List.sortBy (\(_,cav) -> cav.created) present_coding_answers
        templates = List.map (\{answer,question, coding_question,input_type} -> 
                                    CodingAnswerTemplate
                                        (Tuple.first answer)
                                        (Tuple.second answer)
                                        (Tuple.first question)
                                        (Tuple.second question)
                                        (Tuple.first coding_question)
                                        (Tuple.second coding_question)
                                        (Tuple.first input_type)
                                        (Tuple.second input_type)
                                        ) all_coding_answers
        qids_missing : List (String, String)
        qids_missing =
            List.filter (\(answerid,coding_questionid) -> not <| List.member (answerid, coding_questionid) (List.map (\(_,ca) -> (ca.value.answer,ca.value.coding_question)) present_coding_answers)) all_relevant_keys
        
        currentAnswer : Maybe (String, Db.Timestamp Db.CodingAnswer)
        currentAnswer =
            List.Extra.last present_coding_answers
        
        currentQuestion : Maybe CodingAnswerTemplate
        currentQuestion =
            case currentAnswer of 
                Just (caid, cav) ->
                    templates 
                    |> List.filter (\{coding_questionId} -> coding_questionId == cav.value.coding_question)
                    |> List.filter (\{answerId} -> answerId == cav.value.answer)
                    |> List.head
                Nothing ->
                    Nothing
        
        curID =
            Maybe.andThen (\x -> List.Extra.elemIndex x history) currentAnswer

        next : Maybe (String, Db.Timestamp Db.CodingAnswer)
        next =
            Maybe.andThen (\x -> List.Extra.getAt (x + 1) history) curID

        previous : Maybe (String, Db.Timestamp Db.CodingAnswer)
        previous =
            Maybe.andThen (\x -> List.Extra.getAt (x - 1) history) curID

        getMsg answer =
            case answer of
                Just (aid,_) ->
                    Just (Msg.CRUD <| Msg.Access Db.CodingAnswerType aid)

                Nothing ->
                    case List.head qids_missing of
                        Just (answerid, coding_questionid) ->
                            Just
                                (
                                Msg.CRUD
                                    (Msg.CreateRandom Db.CodingAnswerType
                                        [ \canswerid ->
                                            Match.setField
                                                { kind = Db.CodingAnswerType
                                                , attribute = "answer"
                                                , setter = Updater.StringMsg
                                                , id = canswerid
                                                , value = answerid
                                                }
                                        , \canswerid ->
                                            Match.setField
                                                { kind = Db.CodingAnswerType
                                                , attribute = "coding_question"
                                                , setter = Updater.StringMsg
                                                , id = canswerid
                                                , value = coding_questionid
                                                }
                                        ]
                                    )
                            )
                        Nothing ->
                            Nothing

                        
                    
    in
        Model id templates currentAnswer currentQuestion (getMsg previous) (getMsg next)


-- INIT


page : Session.Session -> String -> ( Page.Page Model Msg.Msg, Cmd Msg.Msg )
page session id =
    let
        model =
            { session = session
            , page = init id session.db
            , view = view
            , toMsg = identity
            , subscriptions = Sub.none
            -- , header = Viewer.header
            , update = update

            --            , update = Page.liftupdate update
            }

        
    in
    
    ( Page model, Cmd.none )


    

update : Msg.Msg -> Page.Page Model Msg.Msg -> ( Page.Page Model Msg.Msg, Cmd Msg.Msg )
update message (Page model) =
    ( Page model, Cmd.none )


view : Page Model Msg.Msg -> Viewer.Details Msg.Msg
view (Page.Page pageM) =
    let

        db =
            session.db
        model = pageM.page
        session = pageM.session
        viewportHeight = session.windowSize.height
                        
    in  
    { detailsConfig
        | title = toTitle model
        , user = session.user
        , body =
            \_ -> [
                case model.current of
                    Just (current) ->
                        
                                
                                Element.layout [ height <| px <| viewportHeight - 48, padding 24] <|
                                    Element.column [height fill, width fill]
                                        [ Element.el [ height fill, width fill ] <| viewCodingQuestion db current.coding_questionId current.coding_question model.answer current.answerId model
                                        , Element.row [ Element.alignBottom, width fill]
                                            [ Element.el [Element.alignLeft] <| Element.html
                                                (case model.previous of
                                                    Just prev ->
                                                        Button.raised
                                                            (Button.config |> Button.setOnClick prev)
                                                            "Previous"

                                                    Nothing ->
                                                        text ""
                                                )
                                            , Element.el [ Element.centerX, width fill ] (Element.text "")
                                            , Element.el [ Element.alignRight] <| Element.html
                                                (case model.next of
                                                    Just next ->
                                                        Button.raised
                                                            (Button.config |> Button.setOnClick next)
                                                            "Next"

                                                    Nothing ->
                                                        text ""
                                                )
                                            ]
                                        ]

                             

                    Nothing ->
                        Html.div demoContent
                            [ text "Nothing to do!"
                            ]
            ]
    }


viewCodingQuestion : Db.Database -> String -> Db.Timestamp Db.CodingQuestion -> Maybe ( String, Db.Timestamp Db.CodingAnswer ) -> String -> Model -> Element.Element Msg.Msg
viewCodingQuestion db qid tquestion mbAnswer anid model =
    let
        question = tquestion.value
        mbit =
            Dict.get question.input_type db.input_types
                |> Maybe.map (\x -> x.value)

        mbvalue =
            Maybe.map (\( _, val ) -> val.value.value) mbAnswer

        mbid =
            Maybe.map (\( id, _ ) -> id) mbAnswer

        tonInput =
            case mbid of
                Just answer_id ->
                    \x ->
                        Match.setField
                            { kind = Db.CodingAnswerType
                            , attribute = "value"
                            , setter = Updater.StringMsg
                            , id = answer_id
                            , value = x
                            }

                Nothing ->
                    \x ->
                        Msg.CRUD
                            (Msg.CreateRandom Db.CodingAnswerType
                                [ \answerid ->
                                    Match.setField
                                        { kind = Db.CodingAnswerType
                                        , attribute = "coding_question"
                                        , setter = Updater.StringMsg
                                        , id = answerid
                                        , value = qid
                                        }
                                
                                , \answerid ->
                                    Match.setField
                                        { kind = Db.CodingAnswerType
                                        , attribute = "answer"
                                        , setter = Updater.StringMsg
                                        , id = answerid
                                        , value = anid
                                        }
                                , \answerid ->
                                    Match.setField
                                        { kind = Db.AnswerType
                                        , attribute = "value"
                                        , setter = Updater.StringMsg
                                        , id = answerid
                                        , value = x
                                        }
                                ]
                            )
    in
    Keyed.column [width fill, height fill] [
        ( "title", Element.el [ width fill, height fill] <| Element.el [Element.centerX, Element.centerY {-Background.color (Element.rgb 0.8 0.8 0.8)-},padding 32] <| Element.paragraph [Font.size 32] [Element.text question.text] )
            ,("edit", Keyed.row [width fill, height fill] [
            ("padleft", Element.el [width fill] <| Element.none)
            , (qid, Element.el [ width fill, height fill] <| Element.el [Element.centerY, width fill] <| (case mbit of
                    Nothing ->
                        Element.html <| text "Undefined input type" 
                    Just (ShortAnswer s) ->
                        Element.html <| TextField.filled
                                (TextField.config
                                    |> TextField.setLabel s.label
                                    |> TextField.setValue mbvalue
                                    |> TextField.setPlaceholder s.placeholder
                                    |> TextField.setOnInput tonInput
                                 --|> TextField.setMaxLength s.maxLength
                                 --|> TextField.setMinLength s.minLength
                                )
                          
                        

                    Just (LongAnswer l) ->
                        Element.html <| TextArea.filled
                                (TextArea.config
                                    |> TextArea.setLabel l.label
                                    |> TextArea.setValue mbvalue
                                    |> TextArea.setOnInput tonInput
                                    |> TextArea.setRows l.rows
                                    |> TextArea.setCols l.cols
                                )
                         

                    Just (List _) ->
                        Element.text "List Answer" 
               ))
            , ("padright", Element.el [width fill] <| Element.none)])]





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
