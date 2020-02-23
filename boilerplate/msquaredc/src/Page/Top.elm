module Page.Top exposing (Model, init, update, view, page)

import Browser
import Browser.Events
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events
import Json.Decode
import Ports
import Session
import Utils exposing (..)
import Viewer
import Msg
import Page



{-
   This is the top page - the page that is displayed when the url path is "/"
-}
-- MODEL


type alias Model =
    { localStorageInputField : String
    }



-- INIT


init : Model
init =
    Model ""

page : Session.Session -> (Page.Page Model Msg.TopMsg, Cmd Msg.TopMsg )
page session =
    let 
        model = 
            {session = session,
            page = init,
            view = view,
            toMsg = Msg.Top,
            header = Viewer.header,
            update = update}
    in
        (Page.Page model, Cmd.none )



-- UPDATE



update : Msg.TopMsg -> Page.Page Model Msg.TopMsg -> ( Page.Page Model Msg.TopMsg, Cmd Msg.TopMsg )
update msg (Page.Page model) =
    case msg of
        Msg.NoOp ->
            ( Page.Page model, Cmd.none )

        -- Updates the value of the localStorage input field in the model
        Msg.LocalStorageInputFieldChange input ->
            let 
                oldPage = model.page
                newPage = {oldPage | localStorageInputField = input}
            in
                ( Page.Page { model | page = newPage }, Cmd.none )

        -- Sets the value in local storage (from Set localStorage button, onEnter listener of the localStorage input field)
        Msg.SetLocalStorage ->
            let
                localStorage =
                    { token = model.page.localStorageInputField }

                session =
                    model.session

                newSession = session
                --     { session | localStorage = Just localStorage }
                oldPage = model.page
                newPage = { oldPage | localStorageInputField = ""}
            in
            ( Page.Page { model | page = newPage, session = newSession }, Ports.toLocalStorage localStorage )

        -- Clears localStorage (from Clear localStorage button)
        Msg.ClearLocalStorage ->
            let
                session =
                    model.session

                newSession = session
--                    { session | localStorage = Nothing }
            in
            ( Page.Page { model | session = newSession }, Ports.clearLocalStorage () )



-- VIEW


view : Page.Page Model Msg.TopMsg -> Viewer.Details Msg.TopMsg
view (Page.Page model) =
    { title = toTitle
    , body =
        [ h1 [] [ text "elm-spa-boilerplate" ]
        , div [ class "content" ]
            [ -- Intro and features
              div [] [ text "A simple, no-frills boilerplate for creating delightful Single Page Applications (SPAs) in Elm. Everything you need to get started with no extra clutter. Just clone, compile, and get right to coding!" ]
            , div [] [ text "Some highlights of this boilerplate:" ]
            , highlights

            -- Valid links
            , div [] [ text "Here's some links to try out the client-side routing. Be sure to try using your browser's Back and Forward buttons, and refresh the page anytime!" ]
            , ul [] [ viewLink "/pageone", viewLink "/pagewithsubpage/subpage-name", viewLink "/pagewithsubpage/adpoifjawef" ]

            -- Invalid links demonstrating 404 redirecting (assuming the server is set up to redirect 404 to index.html)
            , div [] [ text "You can handle 404 errors however you'd like - for example, rendering a static page, or routing to the home page. I chose to show a static 404 page - Here's a bunch of links that route there:" ]
            , ul [] [ viewLink "/doesnotexist", viewLink "/invalidpage", viewLink "/pageone/kaldjf", viewLink "/pagewithsubpage/" ]

            -- Demo of localStorage (set, clear, current value)
            , div [] [ text "The required ports, decoder and JS handlers for using localStorage is initalized for you. Check it out:" ]
            , div []
                [ input [ class "input", style "width" "250px", placeholder "Set the value in localStorage...", Html.Events.onInput Msg.LocalStorageInputFieldChange, onEnter Msg.SetLocalStorage, value model.page.localStorageInputField ] [] ]
            , div []
                [ button [ Html.Events.onClick Msg.SetLocalStorage ] [ text "Set localStorage" ]
                , button [ Html.Events.onClick Msg.ClearLocalStorage ] [ text "Clear localStorage" ]
                ]
             , div []
                [ text <|
                    "Current value in localStorage is: "
                        ++ 
                                    "{\"token\": " ++ String.fromInt model.session.windowSize.height ++ "}"

                ] 
            ]
        ]
    }



-- HELPERS


toTitle =
    "Home"


highlights =
    ul []
        [ li [] [ text "Client-side routing that uses pushState navigation and the forward slash `/` as the path separator." ]
        , li [] [ text "Search Engine Optimization (SEO) friendly - unique Title for each page." ]
        , li [] [ text "Support for localStorage, with the necessary ports and JS handlers already initalized." ]
        , li [] [ text "Support for responsive site design by listening for window size changes and always storing window size in the model." ]
        , li [] [ text "Built with webpack." ]
        , li [] [ text "Well-commented code!" ]
        ]



-- Custom event listener for the 'Enter' key being pressed


onEnter : Msg.TopMsg -> Attribute Msg.TopMsg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.Decode.succeed msg

            else
                Json.Decode.fail "not ENTER"
    in
    Html.Events.on "keydown" (Json.Decode.andThen isEnter Html.Events.keyCode)
