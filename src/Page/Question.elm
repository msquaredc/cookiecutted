module Page.Question exposing (..)

import Session
import Page exposing (Page(..))
import Msg
import Viewer exposing (detailsConfig)

type alias Model =
    {
        id : String
    }

page : Session.Session -> String -> ( Page.Page Model Msg.Msg, Cmd Msg.Msg )
page session id =
    let
        model =
            { session = session
            , page = Model id
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
    in
        { detailsConfig
                | title = toTitle model.page
                , user = model.session.user
                , body =
                    \_ -> []}

toTitle : Model -> String
toTitle _ =
    "Home ⧽ Question"