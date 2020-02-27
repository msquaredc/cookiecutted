module Main exposing (Model, init, main, subscriptions, update, view)

-- import Page.NewPage as NewPage
--import Html exposing (..)
--import Html.Attributes exposing (..)
--import Json.Encode

import Browser
import Browser.Events
import Browser.Navigation
import Json.Decode
import Msg
import Page
import Page.Admin as Admin
import Page.PageOne as PageOne
import Page.PageWithSubpage as PageWithSubpage
import Page.Top as Top
import Page.User as User
import Ports
import Session
import Type.Database exposing (database)
import Type.Flags
import Type.IO
import Url
import Url.Parser as Parser exposing ((</>))
import Viewer



-- TYPES
-- Page: each time you need to add/remove a page, this needs to be updated appropriately
-- Each page holds the respective pages model, with the exception of the 404 NotFound page type


type Page
    = NotFound Session.Session
    | Top (Page.Page Top.Model Msg.TopMsg)
    | User (Page.Page User.Model Msg.UserMsg)
      -- | NewPage NewPage.Model
    | PageOne (Page.Page PageOne.Model Msg.PageOneMsg)
    | PageWithSubpage (Page.Page PageWithSubpage.Model Msg.PageWithSubpageMsg)
    | Admin (Page.Page Admin.Model Msg.AdminMsg)



-- MODEL


type alias Model =
    { key : Browser.Navigation.Key -- Required in a Browser.application
    , page : Page
    }



-- INIT
-- To initialize the app, we route the URL to determine what page should be rendered.
-- We also get some information from the flags that will be stored in the Session


init : Type.Flags.Flags -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg.Msg )
init flags url key =
    let
        localStorage =
            Json.Decode.decodeValue Type.Database.database.decoder flags.localStorage

        db =
            Json.Decode.decodeValue Type.Database.database.decoder flags.db

        ( model, cmds ) =
            routeUrl url <| Model key (NotFound <| Session.init flags)
    in
    --  On loading the application, we read form local storage. If the object is incorrectly formatted, clear localStorage
    case localStorage of
        Ok _ ->
            ( model, cmds )

        Err _ ->
            -- If localstorage decoder failed, clear localstorage
            ( model, Cmd.batch [ cmds, Ports.clearLocalStorage () ] )



-- UPDATE


update : Msg.Msg -> Model -> ( Model, Cmd Msg.Msg )
update message model =
    case message of
        -- When a link is clicked anywhere on our page. There are two types of links, external and internal
        Msg.LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    -- If you'd like to use hash-based routing:
                    -- ( model, Nav.pushUrl model.key (Url.toString (toHashUrl url)) )
                    ( model, Browser.Navigation.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Browser.Navigation.load href )

        -- When the URL changes. This could from something like clicking a link or the browser back/forward buttons
        Msg.UrlChanged url ->
            routeUrl url model

        -- Handle this however you'd like for responsive web design! The view in Main.elm and each respective page can change depending on the window size
        Msg.OnWindowResize width height ->
            let
                session =
                    extractSession model

                windowSize =
                    { width = width, height = height }
            in
            updateSession model { session | windowSize = windowSize }

        -- Handle a change in localStorage. Can be modified to your needs
        -- In the boilerplate, I update the session and send a message to the active page with tne new session
        Msg.OnLocalStorageChange msg ->
            let
                -- localStorage =
                --     Json.Decode.decodeValue Type.LocalStorage.decode msg
                db =
                    Json.Decode.decodeValue database.decoder msg

                session =
                    extractSession model

                newSession =
                    case db of
                        Ok success ->
                            { session | db = success }

                        Err _ ->
                            { session | db = database.empty }
            in
            updateSession model newSession

        Msg.OnDbChange msg ->
            let
                db =
                    Json.Decode.decodeValue database.decoder msg

                session =
                    extractSession model

                newSession =
                    case db of
                        Ok success ->
                            { session | db = success }

                        Err _ ->
                            { session | db = database.empty }
            in
            updateSession model newSession

        Msg.User msg ->
            case model.page of
                User m ->
                    mapPageMsg model User (Page.update msg m)

                _ ->
                    ( model, Cmd.none )

        -- The messages below will send a message received in Main.elm to the respective page.
        Msg.Top msg ->
            case model.page of
                Top m ->
                    mapPageMsg model Top (Page.update msg m)

                _ ->
                    ( model, Cmd.none )

        --    NewPage msg ->
        --        case model.page of
        --            NewPage m ->
        --                mapNewPageMsg model (NewPage.update msg m)
        --            _ ->
        --                ( model, Cmd.none )
        Msg.PageOne msg ->
            case model.page of
                PageOne m ->
                    mapPageMsg model PageOne (Page.update msg m)

                -- let
                --     (p, effect) = Page.update m msg
                -- in
                --     ({model | page = PageOne p}, Cmd.map p.toMsg effect)
                _ ->
                    ( model, Cmd.none )

        Msg.PageWithSubpage msg ->
            case model.page of
                PageWithSubpage m ->
                    mapPageMsg model PageWithSubpage (Page.update msg m)

                _ ->
                    ( model, Cmd.none )

        Msg.Viewer _ ->
            ( model, Cmd.none )

        {- ({model| page = Page.updateHeader msg model.page}, Cmd.none) -}
        Msg.Admin msg ->
            --Debug.log (Debug.toString msg) <|
            case model.page of
                Admin m ->
                    let
                        ( newmodel, effect ) =
                            mapPageMsg model Admin (Page.update msg m)

                        session =
                            extractSession newmodel
                    in
                    case msg of
                        Msg.AdminDb _ ->
                            updateSession newmodel (extractSession newmodel)
                                |> (\( x, y ) -> ( x, Cmd.batch [ y, Debug.log "toDb" Ports.toDb (Type.IO.encode database.encoder session.db) ] ))

                        _ ->
                            ( newmodel, effect )

                _ ->
                    ( model, Cmd.none )

        Msg.Db msg ->
            --Debug.todo (Debug.toString msg) <|
            let
                session =
                    extractSession model

                new_db =
                    database.updater msg session.db
                        |> Result.withDefault session.db
            in
            updateSession model { session | db = new_db }



-- VIEW
-- Our view function renders the page depending on which page is active.


view : Model -> Browser.Document Msg.Msg
view model =
    let
        session =
            extractSession model
    in
    case model.page of
        NotFound _ ->
            Viewer.view session never Viewer.notFound Viewer.header

        User m ->
            Page.view m

        Admin m ->
            Page.view m

        Top m ->
            Page.view m

        -- NewPage _ ->
        -- Viewer.view session             NewPageMsg (NewPage.view m) model.route
        PageOne m ->
            Page.view m

        PageWithSubpage m ->
            Page.view m



--Viewer.view session Msg.PageWithSubpage (PageWithSubpage.view m)
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg.Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize Msg.OnWindowResize
        , Ports.onLocalStorageChange Msg.OnLocalStorageChange
        , Ports.onDbChange Msg.OnDbChange
        ]



-- MAIN


main : Program Type.Flags.Flags Model Msg.Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = Msg.UrlChanged
        , onUrlRequest = Msg.LinkClicked
        }



-- FUNCTIONS
-- Helper functions to send a command from Main to a page


mapPageMsg : Model -> (Page.Page a msg -> Page) -> ( Page.Page a msg, Cmd msg ) -> ( Model, Cmd Msg.Msg )
mapPageMsg model map ( Page.Page page, effect ) =
    ( { model | page = map (Page.Page page) }, Cmd.map page.toMsg effect )



-- mapTopMsg : Model -> ( Top.Model, Cmd Msg.TopMsg ) -> ( Model, Cmd Msg.Msg )
-- mapTopMsg model ( m, cmds ) =
--     ( { model | page = Top m }, Cmd.map Msg.Top cmds )
-- mapPageOneMsg : Model -> ( PageOne.PageOne, Cmd Msg.PageOneMsg ) -> ( Model, Cmd Msg.Msg )
-- mapPageOneMsg model ( m, cmds ) =
--     ( { model | page = PageOne m }, Cmd.map Msg.PageOne cmds )
-- mapNewPageMsg : Model -> ( NewPage.Model, Cmd NewPage.Msg ) -> ( Model, Cmd Msg )
-- mapNewPageMsg model ( m, cmds ) =
--     ( { model | page = NewPage m }, Cmd.map NewPageMsg cmds )
-- mapPageWithSubpageMsg : Model -> ( PageWithSubpage.Model, Cmd Msg.PageWithSubpageMsg ) -> ( Model, Cmd Msg.Msg )
-- mapPageWithSubpageMsg model ( m, cmds ) =
--     ( { model | page = PageWithSubpage m }, Cmd.map Msg.PageWithSubpage cmds )
-- Extracts the session from the model


extractSession : Model -> Session.Session
extractSession model =
    let
        getSession =
            \(Page.Page x) -> x.session
    in
    case model.page of
        NotFound session ->
            session

        User m ->
            getSession m

        Top m ->
            getSession m

        Admin m ->
            getSession m

        PageOne m ->
            getSession m

        -- NewPage m ->
        -- m.session
        PageWithSubpage m ->
            getSession m



-- Update the session of the active page (This could be changed to send a OnSessionChange Msg rather than using init)
-- However, I think it's better you design your pages such that initializing the page is equivalent to updating the session!


updateSession : Model -> Session.Session -> ( Model, Cmd Msg.Msg )
updateSession model session =
    case model.page of
        NotFound _ ->
            ( { model | page = NotFound session }, Cmd.none )

        User (Page.Page m) ->
            mapPageMsg model User (User.page session m.page.user_id)

        Top _ ->
            mapPageMsg model Top (Top.page session)

        Admin (Page.Page m) ->
            mapPageMsg model Admin (Admin.page session m.page.subpage)

        PageOne _ ->
            mapPageMsg model PageOne (PageOne.page session)

        -- NewPage m ->
        -- mapNewPageMsg model (NewPage.init session)
        PageWithSubpage (Page.Page m) ->
            mapPageMsg model PageWithSubpage (PageWithSubpage.page session m.page.subpage)



-- ROUTING
-- The following functions create the client-side router. Update "parser" and "paths" for each page you add/remove


routeUrl : Url.Url -> Model -> ( Model, Cmd Msg.Msg )
routeUrl url model =
    let
        session =
            extractSession model

        -- If you'd like to use hash-based routing:
        -- hashUrl =
        --     { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
    in
    -- If you'd like to use hash-based routing:
    -- case Parser.parse (parser model session) hashUrl of
    case Parser.parse (parser model session) url of
        Just success ->
            success

        Nothing ->
            ( { model | page = NotFound session }, Cmd.none )


route : Parser.Parser a b -> a -> Parser.Parser (b -> c) c
route parser_ handler =
    Parser.map handler parser_



-- URL Parser tha maps a URL to a Page, and initializes that page.


parser : Model -> Session.Session -> Parser.Parser (( Model, Cmd Msg.Msg ) -> a) a
parser model session =
    Parser.oneOf
        [ route Parser.top (mapPageMsg model Top (Top.page session))
        , route (Parser.s paths.users) (mapPageMsg model User (User.page session Nothing))
        , route (Parser.s paths.users </> Parser.int) (\user_id -> mapPageMsg model User (User.page session (Just user_id)))
        , route (Parser.s paths.pageOne)
            (mapPageMsg model PageOne (PageOne.page session))
        , route (Parser.s paths.admin </> Admin.parser)
            (\presult -> mapPageMsg model Admin (Admin.page session presult))

        -- , route (Parser.s paths.newPage)
        --     (mapNewPageMsg model (NewPage.init session))
        , route (Parser.s paths.pageWithSubpage </> Parser.string)
            (\subpage -> mapPageMsg model PageWithSubpage (PageWithSubpage.page session subpage))
        ]



--  This holds the paths for each page. Update as needed for each page you add/remove


paths =
    { top = ""
    , users = "user"
    , pageOne = "pageone"
    , pageWithSubpage = "pagewithsubpage"
    , admin = "admin"

    --, newPage = "newpage"
    }



-- Uncomment  this helper function if you need to use hash-based routing.
-- toHashUrl : Url.Url -> Url.Url
-- toHashUrl url =
--     { url | fragment = Just url.path, path = "" }
