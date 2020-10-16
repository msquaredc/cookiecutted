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
import Page.Event as Event
import Page.PageOne as PageOne
import Page.PageWithSubpage as PageWithSubpage
import Page.Questionary as Questionary
import Page.Question as Question
import Page.Study as Study
import Page.Top as Top
import Page.User as User
import Page.Answer as Answer
import Ports
import Random exposing (generate)
import Random.Char exposing (latin)
import Random.String exposing (string)
import Session
import Task exposing (perform)
import Time exposing (now, Posix)
import Material.Snackbar as Snackbar
import Type.Database as Db exposing (database)
import Type.Database.TypeMatching as Match
import Type.Flags
import Type.IO exposing (form2update)
import Type.IO.Setter as Updater
import Url
import Url.Builder
import Url.Parser as Parser exposing ((</>),(<?>))
import Url.Parser.Query as Query
import Viewer
import Url.Parser exposing (query)

 


-- TYPES
-- Page: each time you need to add/remove a page, this needs to be updated appropriately
-- Each page holds the respective pages model, with the exception of the 404 NotFound page type


type Page
    = NotFound Session.Session
    | Top (Page.Page Top.Model Msg.Msg)
    | User (Page.Page User.Model Msg.UserMsg)
      -- | NewPage NewPage.Model
    | PageOne (Page.Page PageOne.Model Msg.PageOneMsg)
    | PageWithSubpage (Page.Page PageWithSubpage.Model Msg.PageWithSubpageMsg)
    | Admin (Page.Page Admin.Model Msg.Msg)
    | Study (Page.Page Study.Model Msg.Msg)
    | Event (Page.Page Event.Model Msg.Msg)
    | Questionary (Page.Page Questionary.Model Msg.Msg)
    | Question (Page.Page Question.Model Msg.Msg)
    | Answer (Page.Page Answer.Model Msg.Msg)



-- MODEL


type alias Model =
    { key : Browser.Navigation.Key -- Required in a Browser.application
    , page : Page
    , header : Viewer.Header
    , time : Maybe Posix
    }



-- INIT
-- To initialize the app, we route the URL to determine what page should be rendered.
-- We also get some information from the flags that will be stored in the Session


init : Type.Flags.Flags -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg.Msg )
init flags url key =
    let
        localStorage =
            Json.Decode.decodeValue Db.database.decoder flags.localStorage

        db =
            Json.Decode.decodeValue Db.database.decoder flags.db

        ( model, cmds ) =
            routeUrl (urlAdaptHash url) <| Model key (NotFound <| Session.init flags) Viewer.header Nothing
        newCmds = Cmd.batch [cmds, perform Msg.Tick now]
    in
    --  On loading the application, we read form local storage. If the object is incorrectly formatted, clear localStorage
    case localStorage of
        Ok _ ->
            ( model, newCmds )

        Err _ ->
            let
                newmodel = {-reportError "Could not load localstorage!"-} model
            in
            -- If localstorage decoder failed, clear localstorage
            ( newmodel, Cmd.batch [ newCmds, Ports.clearLocalStorage () ] )



-- UPDATE


defaultUpdate : Msg.Msg -> ( Model, Cmd Msg.Msg ) -> ( Model, Cmd Msg.Msg )
defaultUpdate message ( model, effect ) =
    let
        session =
            extractSession model

        db =
            session.db
    in
    (\( x, y ) -> ( x, Cmd.batch [ effect, y ] )) <|
        case message of
            -- When a link is clicked anywhere on our page. There are two types of links, external and internal
            Msg.LinkClicked urlRequest ->
                case urlRequest of
                    Browser.Internal url ->
                        -- If you'd like to use hash-based routing:
                        ( model, Browser.Navigation.pushUrl model.key (Url.toString (toHashUrl url)) )

                    -- ( model, Browser.Navigation.pushUrl model.key (Url.toString url) )
                    Browser.External href ->
                        ( model, Browser.Navigation.load href )

            -- When the URL changes. This could from something like clicking a link or the browser back/forward buttons
            Msg.UrlChanged url ->
                routeUrl (urlAdaptHash url) model

            -- Handle this however you'd like for responsive web design! The view in Main.elm and each respective page can change depending on the window size
            Msg.OnWindowResize width height ->
                let
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
                    newdb =
                        Json.Decode.decodeValue database.decoder msg

                    newSession =
                        case newdb of
                            Ok success ->
                                { session | db = success }

                            Err _ ->
                                { session | db = database.empty }
                in
                updateSession model newSession

            Msg.OnDbChange msg ->
                let
                    newdb =
                        Json.Decode.decodeValue database.decoder msg

                    newSession =
                        case newdb of
                            Ok success ->
                                { session | db = success }

                            Err _ ->
                                { session | db = database.empty }
                in
                updateSession model newSession

            Msg.Viewer msg ->
                ( { model | header = Viewer.update msg model.header }, Cmd.none )

            Msg.Db msg ->
                --Debug.todo (Debug.toString msg) <|
                let
                    new_db =
                        database.updater msg session.db
                            |> Result.withDefault session.db
                in
                updateSession model { session | db = new_db }

            Msg.CRUD msg ->
                case msg of
                    Msg.Create kind id callbacks ->
                        let
                            callbacksWithId =
                                List.map (\y -> y id) callbacks

                            newDb =
                                Match.new id kind (Maybe.withDefault "" session.user) db

                            -- |> (\x -> List.foldl (\a b -> a b) (Ok x) updatesWithId)
                        in
                        chainedUpdateAll callbacksWithId <|
                            (\( x, y ) -> ( x, Cmd.batch [ y, Match.setTimestamp kind id "created" ] )) <|
                                case kind of
                                    Db.UserType ->
                                        updateDbSession model { session | user = Just id } newDb
                                        |> chainableUpdate (Msg.SetUser id)

                                    Db.QuestionaryType ->
                                        updateDbSession model session newDb
                                            |> chainableUpdate
                                                (Msg.CRUD
                                                    (Msg.CreateRandom Db.QuestionType
                                                        [ \x ->
                                                            Match.setField
                                                                { kind = Db.QuestionType
                                                                , attribute = "questionary"
                                                                , setter = Updater.StringMsg
                                                                , value = id
                                                                , id = x
                                                                }
                                                        ]
                                                    )
                                                )

                                    _ ->
                                        updateDbSession model session newDb

                    Msg.CreateRandom kind callbacks ->
                        ( model, Random.generate (Msg.CRUD << (\x -> Msg.Create kind x callbacks)) (string 20 latin) )

                    Msg.Update msg_ ->
                        case Db.database.updater msg_ db of
                            Err e ->
                                let
                                    newmodel = reportError (Updater.errToString e) model
                                in
                                    ( newmodel, Cmd.none )

                            Ok newDb ->
                                case msg_ of
                                    Updater.AttributeMsg table (Updater.DictKeyMsg id (Updater.AttributeMsg "value" _)) ->
                                        updateDbSession model session newDb
                                            |> (\( x, y ) ->
                                                    ( x
                                                    , Cmd.batch
                                                        [ y
                                                        , Cmd.map (Msg.CRUD << Msg.Update) <|
                                                            perform
                                                                (\z ->
                                                                    Updater.AttributeMsg table <|
                                                                        Updater.DictKeyMsg id <|
                                                                            Updater.AttributeMsg "modified" <|
                                                                                Updater.IntMsg (Time.posixToMillis z)
                                                                )
                                                                now
                                                        , Cmd.map (Msg.CRUD << Msg.Update) <|
                                                            perform
                                                                (\z ->
                                                                    Updater.AttributeMsg table <|
                                                                        Updater.DictKeyMsg id <|
                                                                            Updater.AttributeMsg "accessed" <|
                                                                                Updater.IntMsg (Time.posixToMillis z)
                                                                )
                                                                now
                                                        ]
                                                    )
                                               )

                                    _ ->
                                        updateDbSession model session newDb
                    
                    Msg.Access kind id ->
                        ( model, Cmd.batch [ Cmd.map (Msg.CRUD << Msg.Update) <|
                                        perform
                                            (\z ->
                                                Updater.AttributeMsg (Match.toStringPlural kind) <|
                                                    Updater.DictKeyMsg id <|
                                                        Updater.AttributeMsg "accessed" <|
                                                            Updater.IntMsg (Time.posixToMillis z)
                                            )
                                            now
                                            ])


                    Msg.UpdateAll updates ->
                        case List.foldl (Result.andThen << Db.database.updater) (Ok db) updates of
                            Err e ->
                                let
                                    newmodel = reportError (Updater.errToString e) model
                                in
                                    ( newmodel, Cmd.none )
                            Ok newDb ->
                                updateDbSession model session newDb

                    Msg.Delete kind id ->
                        let
                            newDb = Match.delete id kind db
                        in
                            updateDbSession model session newDb
                    -- Msg.SwapAttributes kind (first, second) attribute ->
                    --     let
                    --         fvalue = db
                    --     in
                       
                    --     (model, Cmd.none)
            Msg.Form msg ->
                case form2update msg of
                    Just dbmsg ->
                        update (Msg.CRUD <| Msg.Update dbmsg) model

                    Nothing ->
                        ( model, Cmd.none )

            Msg.Follow kind id ->
                ( model, Browser.Navigation.pushUrl model.key <| "#" ++ Url.Builder.absolute [ Match.toString kind, id ] [] )
            
            Msg.FollowSubpage kind id subpages qparam -> 
                ( model, Browser.Navigation.pushUrl model.key <| "#" ++ Url.Builder.absolute ([ Match.toString kind, id] ++ subpages)  qparam )

            Msg.SetUser id ->
                    let
                        newSession =
                            { session | user = Just id }
                    in
                        updateSession model newSession
                        |> (\(x, y) -> (x, Cmd.batch [y 
                                                     , 
                                                            perform
                                                                (\z ->
                                                                    Match.setField 
                                                                        { kind = Db.UserType
                                                                        , attribute = "last_login"
                                                                        , setter = Updater.IntMsg << Time.posixToMillis
                                                                        , value = z
                                                                        , id = id
                                                                        }
                                                                )
                                                                now]))
            
            Msg.Back ->
                (model, Browser.Navigation.back model.key 1)

            Msg.Tick time ->
                ({model | time = Just time}, Cmd.none)
            
            Msg.SnackbarClosed messageId ->
                let
                    oldheader = model.header
                    newheader = {oldheader | queue = Snackbar.close messageId oldheader.queue}
                in
                
                ({model | header = newheader}, Cmd.none)
            

            _ ->
                ( model, Cmd.none )


updateAll : List Msg.Msg -> Model -> ( Model, Cmd Msg.Msg )
updateAll messages model =
    chainedUpdateAll messages ( model, Cmd.none )


chainedUpdateAll : List Msg.Msg -> ( Model, Cmd Msg.Msg ) -> ( Model, Cmd Msg.Msg )
chainedUpdateAll messages ( model, effect ) =
    List.foldl chainableUpdate ( model, effect ) messages


chainableUpdate : Msg.Msg -> ( Model, Cmd Msg.Msg ) -> ( Model, Cmd Msg.Msg )
chainableUpdate message ( model, effect ) =
    update message model
        |> (\( x, y ) -> ( x, Cmd.batch [ effect, y ] ))


update : Msg.Msg -> Model -> ( Model, Cmd Msg.Msg )
update message model =
    case model.page of
        User m ->
            case message of
                Msg.User msg ->
                    mapPageMsg model User (Page.update msg m)

                _ ->
                    defaultUpdate message ( model, Cmd.none )

        -- Msg.User msg ->
        --     case model.page of
        --         User m ->
        --             mapPageMsg model User (Page.update msg m)
        --         _ ->
        --             ( model, Cmd.none )
        -- The messages below will send a message received in Main.elm to the respective page.
        Top m ->
            mapPageMsg model Top (Page.update message m)
                |> defaultUpdate message

        -- case message of
        --     Msg.Top msg ->
        --     _ ->
        --         defaultUpdate message (model, Cmd.none)
        --    NewPage msg ->
        --        case model.page of
        --            NewPage m ->
        --                mapNewPageMsg model (NewPage.update msg m)
        --            _ ->
        --                ( model, Cmd.none )
        PageOne m ->
            case message of
                Msg.PageOne msg ->
                    mapPageMsg model PageOne (Page.update msg m)

                -- let
                --     (p, effect) = Page.update m msg
                -- in
                --     ({model | page = PageOne p}, Cmd.map p.toMsg effect)
                _ ->
                    defaultUpdate message ( model, Cmd.none )

        PageWithSubpage m ->
            case message of
                Msg.PageWithSubpage msg ->
                    mapPageMsg model PageWithSubpage (Page.update msg m)

                _ ->
                    defaultUpdate message ( model, Cmd.none )

        {- ({model| page = Page.updateHeader msg model.page}, Cmd.none) -}
        Admin m ->
            --Debug.log (Debug.toString msg) <|
            let
                ( newmodel, effect ) =
                    --Admin.update message
                    mapPageMsg model Admin (Page.update message m)

                session =
                    extractSession newmodel
            in
            case message of
                Msg.Admin (Msg.AdminDb _) ->
                    updateSession newmodel (extractSession newmodel)
                        |> (\( x, y ) -> ( x, Cmd.batch [ y, Ports.toDb (Type.IO.encode database.encoder session.db) ] ))

                Msg.Search s ->
                    let
                        replacer =
                            Browser.Navigation.replaceUrl model.key <| Admin.url m ++ "?q=" ++ s
                    in
                    defaultUpdate message ( newmodel, Cmd.batch [ effect, replacer ] )

                _ ->
                    defaultUpdate message ( newmodel, effect )

        Study m ->
            
            mapPageMsg model Study (Page.update message m)
                |> defaultUpdate message

        Event m ->
            case message of 
                    Msg.Event (Msg.AnswerQuestions newmodel) ->
                        let
                            session = extractSession model
                            (newpage, effect) = Answer.page session newmodel
                        in
                            ({model| page = Answer <| newpage}, effect)
                    _ ->
                        mapPageMsg model Event (Page.update message m)
                            |> defaultUpdate message

        Questionary m ->
            mapPageMsg model Questionary (Page.update message m)
            |> defaultUpdate message
        
        Question m ->
            mapPageMsg model Question (Page.update message m)
                |> defaultUpdate message

        Answer m ->
            mapPageMsg model Answer (Page.update message m)
                |> defaultUpdate message

        NotFound _ ->
            defaultUpdate message ( model, Cmd.none )



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
            Viewer.view session never Viewer.notFound Viewer.header model.time

        User m ->
            Page.view m model.header model.time

        Admin m ->
            Page.view m model.header model.time

        Top m ->
            Page.view m model.header model.time

        -- NewPage _ ->
        -- Viewer.view session             NewPageMsg (NewPage.view m) model.route
        PageOne m ->
            Page.view m model.header model.time

        PageWithSubpage m ->
            Page.view m model.header model.time

        Study m ->
            Page.view m model.header model.time

        Event m ->
            Page.view m model.header model.time

        Questionary m ->
            Page.view m model.header model.time
        
        Question m ->
            Page.view m model.header model.time
        
        Answer m ->
            Page.view m model.header model.time



--Viewer.view session Msg.PageWithSubpage (PageWithSubpage.view m)
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg.Msg
subscriptions {page} =
    Sub.batch <|
        [case page of
            Questionary (Page.Page m) ->
                Sub.map m.toMsg m.subscriptions
            _ ->
                Sub.none
        , Browser.Events.onResize Msg.OnWindowResize
        , Ports.onLocalStorageChange Msg.OnLocalStorageChange
        , Ports.onDbChange Msg.OnDbChange
        , Time.every 1000 Msg.Tick
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

        Study m ->
            getSession m

        Event m ->
            getSession m

        Questionary m ->
            getSession m
        
        Question m ->
            getSession m

        Answer m ->
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
            Top.page session
                |> (\( x, y ) -> ( { model | page = Top x }, y ))

        -- mapPageMsg model Top (Top.page session)
        Admin (Page.Page m) ->
            Admin.page session m.page.subpage
                |> (\( x, y ) -> ( { model | page = Admin x }, y ))

        PageOne _ ->
            mapPageMsg model PageOne (PageOne.page session)

        -- NewPage m ->
        -- mapNewPageMsg model (NewPage.init session)
        PageWithSubpage (Page.Page m) ->
            mapPageMsg model PageWithSubpage (PageWithSubpage.page session m.page.subpage)

        Study (Page.Page m) ->
            Study.page session m.page.id m.page.nameFocus
                |> (\( x, y ) -> ( { model | page = Study x }, y ))

        Event (Page.Page m) ->
            Event.page session m.page.page m.page.id m.page.nameFocus
                |> (\( x, y ) -> ( { model | page = Event x }, y ))

        Questionary (Page.Page m) ->
            Questionary.page session m.page.id m.page.focus (Just m.page.questions) m.page.dnd
                |> (\( x, y ) -> ( { model | page = Questionary x }, y ))
        
        Question (Page.Page m) ->
            Question.page session m.page.id
                |> (\( x, y ) -> ( { model | page = Question x }, y ))
        
        Answer (Page.Page m) ->
            Answer.page session m.page
                |> (\( x, y ) -> ( { model | page = Answer x }, y ))


updateDb : Db.Database -> ( Model, Cmd Msg.Msg ) -> ( Model, Cmd Msg.Msg )
updateDb db ( model, effect ) =
    ( model, Cmd.batch [ effect, Ports.toDb (Type.IO.encode database.encoder db) ] )


updateDbSession : Model -> Session.Session -> Db.Database -> ( Model, Cmd Msg.Msg )
updateDbSession model session db =
    updateSession model { session | db = db }
        |> updateDb db



-- ROUTING
-- The following functions create the client-side router. Update "parser" and "paths" for each page you add/remove
testMethod =
    case Url.fromString "http://localhost:3000/event/oLFlGAGBkkaZDCTsnmOA/answer?tsid=a" of 
        Nothing ->
            Nothing
        Just oldUrl ->
            let
                hashUrl = { oldUrl | path = Maybe.withDefault "" oldUrl.fragment, fragment = Nothing }
                func x y = "yes"
            in
                Parser.parse (Parser.map func (Parser.s paths.event </> Parser.string </> Parser.s "answer" <?> Query.custom "tsid" identity)) oldUrl
    --Parser.parse (Parser.s paths.event </> Parser.string </> Answer.parser )

routeUrl : Url.Url -> Model -> ( Model, Cmd Msg.Msg )
routeUrl url model =
    let
        session =
            extractSession model

        -- If you'd like to use hash-based routing:
        hashUrl =
            { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
    in
    -- If you'd like to use hash-based routing:
    
    --case Parser.parse (parser model session) hashUrl of
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
        , route (Parser.s paths.study </> Parser.string)
            (\id -> mapPageMsg model Study (Study.page session id False))
        --
        , route (Parser.s paths.event </> Parser.string </> Answer.parser )
            (\eid answer_result -> 
                case answer_result eid of
                    Just amodel ->
                        mapPageMsg model Answer (Answer.page
                            session 
                            amodel)
                    Nothing ->
                        mapPageMsg model Event (
                        Event.page
                            session 
                            Msg.EventSettings
                            eid 
                            False)

            )
                  
        , route (Parser.s paths.event </> Parser.string </> Parser.s "people")
            (\id -> mapPageMsg model Event (
                Event.page
                    session 
                    Msg.EventPeople
                    id 
                    False))
        , route (Parser.s paths.event </> Parser.string </> Parser.s "settings")
            (\id -> mapPageMsg model Event (
                Event.page
                    session 
                    Msg.EventSettings
                    id 
                    False))
            
        , route (Parser.s paths.event </> Parser.string)
            (\id -> mapPageMsg model Event (
                Event.page
                    session 
                    Msg.EventOverview
                    id 
                    False))
        , route (Parser.s paths.questionary </> Parser.string)
            (\id -> mapPageMsg model Questionary (Questionary.page session id Questionary.defaultFokus Nothing Viewer.system.model))
        , route (Parser.s paths.question </> Parser.string)
            (\id -> mapPageMsg model Question (Question.page session id))
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
    , study = "study"
    , event = "event"
    , questionary = "questionary"
    , question = "question"

    --, newPage = "newpage"
    }



-- Uncomment  this helper function if you need to use hash-based routing.


toHashUrl : Url.Url -> Url.Url
toHashUrl url =
    { url | fragment = Just url.path, path = "" }


reportError : String -> Model -> Model 
reportError msg model =
    let
        oldheader = model.header
        message = Snackbar.message msg
                    |> Snackbar.setStacked True
        newQueue = Snackbar.addMessage message oldheader.queue
        newheader = {oldheader | queue = newQueue}
    in
        {model|header = newheader}

urlAdaptHash : Url.Url -> Url.Url
urlAdaptHash url =
    let
        mbnewUrl = url
                |> Url.toString 
                |> String.replace "/#/" "/"
                |> Url.fromString
    in
        Maybe.withDefault url mbnewUrl