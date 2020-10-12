module Page.Question exposing (..)

import Session
import Page exposing (Page(..))
import Msg
import Viewer exposing (detailsConfig)
import Html exposing (Html, text, p)
import Material.LayoutGrid exposing (layoutGrid, inner, cell)
import Material.Typography as Typography exposing (typography)
import Material.TextField as TextField
import Material.Select as Select
import Material.Select.Item as SelectItem
import Viewer.EditableText as EditableText
import Type.Database.TypeMatching as Match
import Type.Database as Db
import Type.IO.Setter as Updater
import Type.Database.InputType as IT 
import Time exposing (Posix)
import Dict
import Viewer.OrderAwareList exposing (orderAwareList, OrderAware)

type alias Model =
    {
        id : String,
        short : IT.ShortAnswerConfig,
        long : IT.LongAnswerConfig,
        list : IT.ListConfig,
        drop : ITDrop
    }

type ITDrop =
    SA
    | LA
    | L

init : Db.Database -> String -> Model
init db id =
    let
        emptymodel = Model 
                        id
                        IT.shortAnswerConfig.empty
                        IT.longAnswerConfig.empty
                        IT.listConfig.empty
                        SA
    in 
    case Dict.get id db.questions of 
        Just element ->
            case element.value.input_type of
                IT.ShortAnswer a ->
                    {emptymodel | short = a, drop = SA}
                IT.LongAnswer a ->
                    {emptymodel | long = a, drop = LA}
                IT.List a -> 
                    {emptymodel | list = a, drop = L}
        Nothing ->
            emptymodel
    
        

page : Session.Session -> String -> ( Page.Page Model Msg.Msg, Cmd Msg.Msg )
page session id =
    let
        model =
            { session = session
            , page = init session.db id
            , view = view
            , toMsg = identity

            -- , header = Viewer.header
            , update = update

            --            , update = Page.liftupdate update
            }
    in
    ( Page model, Cmd.none )

update : Msg.Msg -> Page.Page Model Msg.Msg -> ( Page.Page Model Msg.Msg, Cmd Msg.Msg )
update message (Page model) =
    let
        oldmodel =
            model.page
        updatePage x = 
            Page {model| page = x}
    in
    case message of
        Msg.Question msg ->
            (Page model, Cmd.none)
        _ ->
            (Page model, Cmd.none)

view : Page Model Msg.Msg -> Viewer.Details Msg.Msg
view (Page.Page model) =
    let
        db =
            model.session.db
        mbInfos =
            relatedData model.page.id db
    in
        { detailsConfig
                | title = toTitle model.page
                , user = model.session.user
                , body =
                    \_ -> [
                        case mbInfos of 
                            Just infos ->
                                layoutGrid [ Typography.typography ]
                                    [ inner [] <|
                                        [ cell [{- LG.span12 -}]
                                            [ Html.h1 [ Typography.headline5 ]
                                                [ text "Question"
                                                ]

                                            -- , p [][ text <| "Location:" ++ infos.location]
                                            , p [] [ text <| "Text: " {-++ viewStudy infos.study model.session.user-} ]
                                            , TextField.filled
                                                (TextField.config
                                                    |> TextField.setLabel (Just "Question text")
                                                    |> TextField.setValue (Just infos.text)
                                                    |> TextField.setOnInput (
                                                        \x ->
                                                            Match.setField
                                                                { kind = Db.QuestionType
                                                                , attribute = "text"
                                                                , setter = Updater.StringMsg
                                                                , id = model.page.id
                                                                , value = x
                                                                }
                                                    )
                                                )
                                            ]
                                        , cell [] [
                                            viewInputTypeSelection model.page infos.input_type
                                        ]
                                        ]
                                    ]
                            Nothing ->
                                text "got No Infos"
                        
                        
                    ]}

       

toTitle : Model -> String
toTitle _ =
    "Home ⧽ Question"


type alias RelatedData =
    { id : String
    , text : String
    , input_type : IT.InputType
    --, question : ( String, Maybe Db.Question)
    --, coding_questions : List (OrderAware Db.CodingQuestion)
    , created : Posix
    , creator : ( String, Maybe Db.User )
    , updated : Posix
    }


relatedData : String -> Db.Database -> Maybe RelatedData
relatedData id db =
    case Dict.get id db.questions of 
        Just timestampedQuestion ->
            let
                coding_questions =
                    {- List.sortBy (\( _, y ) -> y.index) <|-}
                        List.filter (\( _, y ) -> y.question == id) <|
                            List.map (\( x, y ) -> ( x, y.value )) <|
                                Dict.toList db.coding_questionnaries

                question =
                    timestampedQuestion.value
                
            in
            Just
                { id = id
                , text = question.text
                , input_type = question.input_type
                --, question = ( questionary.study, Maybe.map .value <| Dict.get questionary.study db.studie)
                --, max_index = List.maximum <| List.map (\( _, x ) -> x.index) coding_questions
                --, coding_questions = orderAwareList coding_questions
                , created = Time.millisToPosix timestampedQuestion.created
                , creator = ( timestampedQuestion.creator, Maybe.map .value <| Dict.get timestampedQuestion.creator db.users )
                , updated = Time.millisToPosix timestampedQuestion.modified
                }

        Nothing ->
            Nothing

viewInputTypeSelection : Model -> IT.InputType -> Html Msg.Msg
viewInputTypeSelection model input_type = 
    Select.outlined
        (Select.config
            |> Select.setLabel (Just "Input Type")
            |> Select.setSelected (Just model.drop)
            |> Select.setOnChange (\drop -> Msg.Question <| Msg.SetInputType <|
                                                                case drop of
                                                                    SA -> IT.ShortAnswer model.short
                                                                    LA -> IT.LongAnswer model.long
                                                                    L -> IT.List model.list)
        )
        (SelectItem.selectItem
            (SelectItem.config { value = SA })
            [ text "Short Answer" ]
        )
        [ SelectItem.selectItem
            (SelectItem.config { value = LA })
            [ text "Long Answer" ]
        ]