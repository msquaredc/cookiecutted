module Page.Answer exposing (Model, RelatedData, page, parser)

--import Html.Keyed as Keyed

import Dict
import Element exposing (fill, height, padding, px, width)
import Element.Font as Font
import Element.Keyed as Keyed
import Html exposing (text)
import Html.Attributes exposing (style)
import List.Extra
import Material.Button as Button
import Material.TextArea as TextArea
import Material.TextField as TextField
import Msg
import Page exposing (Page(..))
import Session
import Type.Database as Db
import Type.Database.InputType exposing (InputType(..))
import Type.Database.TypeMatching as Match
import Type.IO.Internal exposing (Id, box, unbox)
import Type.IO.Setter as Updater
import Url.Parser as Parser exposing ((</>))
import Url.Parser.Query as Query
import Viewer exposing (detailsConfig)


type alias Model =
    { questionary : String
    , test_subject : String
    , event : String
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
            , subscriptions = Sub.none

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
                           )
        -}
    in
    {- case List.head infos.qids_missing of
       Nothing ->
           ( Page model, Cmd.none )
       Just v ->
           ( Page model, v
                           |> toMsg
                           |> toCmd)
    -}
    ( Page model, Cmd.none )


parser : Parser.Parser ((String -> Maybe Model) -> a) a
parser =
    Parser.s "answer"
        </> (Parser.query <|
                Query.map2
                    (\qid tsid ->
                        \eid ->
                            Maybe.map2 Model qid tsid
                                |> Maybe.map (\x -> x eid)
                    )
                    (Query.string "qid")
                    (Query.string "tsid")
            )



{- let
       page2parser : Db.Type -> Parser.Parser (SubPage -> b) b
       page2parser subpage =
           Parser.map (Query subpage) (Parser.s (Match.toString subpage) <?> Query.string "q")

       page2edit : Db.Type -> Parser.Parser (SubPage -> b) b
       page2edit subpage =
           Parser.map (Edit subpage) (Parser.s (Match.toString subpage) </> Parser.string)
   in
   Parser.oneOf
       (Parser.map Home Parser.top :: List.map page2parser Match.types ++ List.map page2edit Match.types)
-}


update : Msg.Msg -> Page.Page Model Msg.Msg -> ( Page.Page Model Msg.Msg, Cmd Msg.Msg )
update message (Page model) =
    ( Page model, Cmd.none )


view : Page Model Msg.Msg -> Viewer.Details Msg.Msg
view (Page.Page model) =
    let
        infos =
            relatedData db model.page

        db =
            model.session.db

        viewportHeight =
            model.session.windowSize.height
    in
    { detailsConfig
        | title = toTitle model.page
        , user = model.session.user
        , body =
            \_ ->
                [ case infos.currentQuestionId of
                    Just qid ->
                        case Dict.get (unbox qid) <| Dict.fromList (List.map (\( a, b ) -> ( unbox a, b )) infos.questions) of
                            Just question ->
                                Element.layout [ height <| px <| viewportHeight - 48, padding 24 ] <|
                                    Element.column [ height fill, width fill ]
                                        [ Element.el [ height fill, width fill ] <| viewQuestion db qid question infos.currentAnswer model.page
                                        , Element.row [ Element.alignBottom, width fill ]
                                            [ Element.el [ Element.alignLeft ] <|
                                                Element.html
                                                    (case infos.previous of
                                                        Just prev ->
                                                            Button.raised
                                                                (Button.config |> Button.setOnClick prev)
                                                                "Previous"

                                                        Nothing ->
                                                            text ""
                                                    )
                                            , Element.el [ Element.centerX, width fill ] (Element.text "")
                                            , Element.el [ Element.alignRight ] <|
                                                Element.html
                                                    (case infos.next of
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
                                text "Question not found"

                    Nothing ->
                        Html.div demoContent
                            [ text "Nothing to do!"
                            ]
                ]
    }


viewQuestion : Db.Database -> Id Db.Question String -> Db.Question -> Maybe ( Id Db.Answer String, Db.Answer ) -> Model -> Element.Element Msg.Msg
viewQuestion db qid question mbAnswer model =
    let
        mbit =
            Dict.get (unbox question.input_type) db.input_types
                |> Maybe.map (\x -> x.value)

        mbvalue =
            Maybe.map (\( _, val ) -> val.value) mbAnswer

        mbid =
            Maybe.map (\( id, _ ) -> id) mbAnswer

        tonInput =
            case mbid of
                Just answer_id ->
                    \x ->
                        Match.setField
                            { kind = Db.AnswerType
                            , attribute = "value"
                            , setter = Updater.StringMsg
                            , id = answer_id
                            , value = x
                            }

                Nothing ->
                    \x ->
                        Msg.CRUD
                            (Msg.CreateRandom Db.AnswerType
                                [ \answerid ->
                                    Match.setField
                                        { kind = Db.AnswerType
                                        , attribute = "question"
                                        , setter = Updater.StringMsg
                                        , id = box answerid
                                        , value = unbox qid
                                        }
                                , \answerid ->
                                    Match.setField
                                        { kind = Db.AnswerType
                                        , attribute = "event"
                                        , setter = Updater.StringMsg
                                        , id = box answerid
                                        , value = model.event
                                        }
                                , \answerid ->
                                    Match.setField
                                        { kind = Db.AnswerType
                                        , attribute = "test_subject"
                                        , setter = Updater.StringMsg
                                        , id = box answerid
                                        , value = model.test_subject
                                        }
                                , \answerid ->
                                    Match.setField
                                        { kind = Db.AnswerType
                                        , attribute = "value"
                                        , setter = Updater.StringMsg
                                        , id = box answerid
                                        , value = x
                                        }
                                ]
                            )
    in
    Keyed.column [ width fill, height fill ]
        [ ( "title"
          , Element.el [ width fill, height fill ] <|
                Element.el
                    [ Element.centerX
                    , Element.centerY

                    {- Background.color (Element.rgb 0.8 0.8 0.8) -}
                    , padding 32
                    ]
                <|
                    Element.paragraph [ Font.size 32 ] [ Element.text question.text ]
          )
        , ( "edit"
          , Keyed.row [ width fill, height fill ]
                [ ( "padleft", Element.el [ width fill ] <| Element.none )
                , ( unbox qid
                  , Element.el [ width fill, height fill ] <|
                        Element.el [ Element.centerY, width fill ] <|
                            case mbit of
                                Nothing ->
                                    Element.html <| text "Undefined input type"

                                Just (ShortAnswer s) ->
                                    Element.html <|
                                        TextField.filled
                                            (TextField.config
                                                |> TextField.setLabel s.label
                                                |> TextField.setValue mbvalue
                                                |> TextField.setPlaceholder s.placeholder
                                                |> TextField.setOnInput tonInput
                                             --|> TextField.setMaxLength s.maxLength
                                             --|> TextField.setMinLength s.minLength
                                            )

                                Just (LongAnswer l) ->
                                    Element.html <|
                                        TextArea.filled
                                            (TextArea.config
                                                |> TextArea.setLabel l.label
                                                |> TextArea.setValue mbvalue
                                                |> TextArea.setOnInput tonInput
                                                |> TextArea.setRows l.rows
                                                |> TextArea.setCols l.cols
                                            )

                                Just (List _) ->
                                    Element.text "List Answer"
                  )
                , ( "padright", Element.el [ width fill ] <| Element.none )
                ]
          )
        ]


type alias RelatedData =
    { questions : List ( Id Db.Question String, Db.Question )
    , answers : List ( Id Db.Answer String, Db.Answer )
    , qids_missing : List (Id Db.Question String)
    , currentQuestionId : Maybe (Id Db.Question String)
    , currentAnswer : Maybe ( Id Db.Answer String, Db.Answer )
    , next : Maybe Msg.Msg
    , previous : Maybe Msg.Msg
    }


relatedData : Db.Database -> Model -> RelatedData
relatedData db model =
    let
        questions =
            db.questions
                |> Dict.filter (\_ val -> val.value.questionary == box model.questionary)
                |> Dict.filter (\_ val -> Dict.member (unbox val.value.input_type) db.input_types)
                |> Dict.toList
                |> List.sortBy (\( _, val ) -> val.value.index)
                |> List.map (\( id, val ) -> ( box id, val.value ))

        qids =
            List.map (\( x, _ ) -> x) questions

        answers =
            List.map (\qid -> Dict.filter (\_ val -> val.value.question == qid) db.answers) qids
                |> List.map (Dict.filter (\_ val -> val.value.event == box model.event))
                |> List.map (Dict.filter (\_ val -> val.value.test_subject == box model.test_subject))
                |> List.map Dict.toList
                |> List.map (List.sortBy (\( _, val ) -> val.created))
                |> List.filterMap List.Extra.last
                |> List.sortBy (\( _, val ) -> val.accessed)
                |> List.map (\( a, b ) -> ( box a, b ))

        qids_present =
            List.map (\( _, val ) -> val.value.question) answers

        qids_missing =
            List.filter (\id -> not <| List.member id qids_present) qids

        currentQuestion =
            case List.Extra.last qids_present of
                Just qid ->
                    Just qid

                Nothing ->
                    List.head qids_missing

        answerFromId id =
            answers
                |> List.filter (\( _, val ) -> Just val.value.question == id)
                |> List.map (\( aid, val ) -> ( aid, val.value ))
                |> List.head

        currentAnswer =
            answerFromId currentQuestion

        curID : Maybe Int
        curID =
            Maybe.andThen (\x -> List.Extra.elemIndex x qids) currentQuestion

        nextID : Maybe (Id Db.Question String)
        nextID =
            Maybe.andThen (\x -> List.Extra.getAt (x + 1) qids) curID

        nextAnswer =
            answerFromId nextID

        prevID =
            Maybe.andThen (\x -> List.Extra.getAt (x - 1) qids) curID

        prevAnswer =
            answerFromId prevID

        getMsg : Maybe (Id Db.Question String) -> Maybe ( Id Db.Answer String, Db.Answer ) -> Maybe Msg.Msg
        getMsg id answer =
            case answer of
                Just ( aid, _ ) ->
                    Just (Msg.CRUD <| Msg.Access Db.AnswerType (unbox aid))

                Nothing ->
                    Maybe.map
                        (\qid ->
                            Msg.CRUD
                                (Msg.CreateRandom Db.AnswerType
                                    [ \answerid ->
                                        Match.setField
                                            { kind = Db.AnswerType
                                            , attribute = "question"
                                            , setter = Updater.StringMsg
                                            , id = box answerid
                                            , value = unbox qid
                                            }
                                    , \answerid ->
                                        Match.setField
                                            { kind = Db.AnswerType
                                            , attribute = "event"
                                            , setter = Updater.StringMsg
                                            , id = box answerid
                                            , value = model.event
                                            }
                                    , \answerid ->
                                        Match.setField
                                            { kind = Db.AnswerType
                                            , attribute = "test_subject"
                                            , setter = Updater.StringMsg
                                            , id = box answerid
                                            , value = model.test_subject
                                            }
                                    ]
                                )
                        )
                        id
    in
    { questions =
        questions
    , answers =
        answers
            |> List.map (\( id, val ) -> ( id, val.value ))
    , qids_missing = qids_missing
    , currentQuestionId = currentQuestion
    , currentAnswer = currentAnswer
    , next = getMsg nextID nextAnswer
    , previous = getMsg prevID prevAnswer
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
