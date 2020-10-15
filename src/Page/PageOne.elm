module Page.PageOne exposing (Model, init, update, view, page)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Session
import Viewer exposing (detailsConfig)
import Msg
import Page



-- MODEL


type alias Model =
    {}



-- INIT
init : Model
init = {}

page : Session.Session -> (Page.Page Model Msg.PageOneMsg, Cmd Msg.PageOneMsg )
page session =
    let 
        model = 
            {session = session,
            page = init,
            view = view,
            toMsg = Msg.PageOne,
            subscriptions = Sub.none,
            -- header = Viewer.header,
            update = Page.liftupdate update }
    in
        ( Page.Page model, Cmd.none )



-- UPDATE
update : Msg.PageOneMsg -> Model -> ( Model, Cmd Msg.PageOneMsg )
update msg model =
    case msg of
        Msg.PageOneNothing ->
            ( model, Cmd.none )



-- VIEW


view : Page.Page Model Msg.PageOneMsg -> Viewer.Details Msg.Msg
view model =
    {detailsConfig | title = toTitle
    , body = \_ ->
        [ h1 [] [ text "elm-spa-boilerplate - Page One" ]
        , div [] [ text "A beautiful, completely empty page in your application." ]
        ]
    }



-- HELPERS


toTitle =
    "Page One"
