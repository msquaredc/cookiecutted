module Page.Admin exposing (Model, init, page, parser, update, url, view)

--import Browser

import Dict
import Html exposing (a, div, h1, h3, li, p, text, ul)
import Html.Attributes exposing (class, href)
import Material.Button as Button exposing (config)
import Material.DataTable as DataTable
    exposing
        ( dataTable
        , cell
        , config
        , row
        )
import Msg exposing (AdminMsg)
import Page exposing (Page(..))
import Ports
import Session
import String.Extra
import StringDistance exposing (sift3Distance)
import Type.Database as Db
import Type.Database.TypeMatching as Match
import Type.IO exposing (form2update)
import Type.IO.Form as Form
import Type.IO.Setter as Update
import Type.IO.ToString as ToString
import Url.Parser as Parser exposing ((</>), (<?>))
import Url.Parser.Query as Query
import Viewer exposing (detailsConfig)



{-
   This is a page with subpages. You can change the behaviour depending on the subpage path!
-}
-- MODEL


type alias Model =
    { subpage : SubPage
    , textfield : String
    , error : Maybe String
    }


type SubPage
    = Home
    | Query Db.Type (Maybe String)
    | Edit Db.Type String



-- INIT


init : SubPage -> Model
init =
    \x -> Model x "" Nothing


page : Session.Session -> SubPage -> ( Page.Page Model Msg.Msg, Cmd Msg.Msg )
page session table =
    let
        model =
            { session = session
            , page = init table
            , view = view
            , toMsg = identity

            -- , header = Viewer.header
            , update = update

            --            , update = Page.liftupdate update
            }
    in
    ( Page model, Cmd.none )


parser : Parser.Parser (SubPage -> a) a
parser =
    let
        page2parser : Db.Type -> Parser.Parser (SubPage -> b) b
        page2parser subpage =
            Parser.map (Query subpage) (Parser.s (Match.toString subpage) <?> Query.string "q")

        page2edit : Db.Type -> Parser.Parser (SubPage -> b) b
        page2edit subpage =
            Parser.map (Edit subpage) (Parser.s (Match.toString subpage) </> Parser.string)
    in
    Parser.oneOf
        (Parser.map Home Parser.top :: List.map page2parser Match.types ++ List.map page2edit Match.types)



-- UPDATE


update : Msg.Msg -> Page.Page Model Msg.Msg -> ( Page.Page Model Msg.Msg, Cmd Msg.Msg )
update message (Page model) =
    case message of
        Msg.Admin msg ->
            case msg of
                Msg.AdminForm msg_ ->
                    case form2update msg_ of
                        Just dbmsg ->
                            --Debug.log "form2update" <
                            updateDb (Db.database.updater dbmsg) (Page model)

                        Nothing ->
                            ( Page model, Cmd.none )

                Msg.AdminDb dbmsg ->
                    case dbmsg of
                        Msg.Create kind index _ ->
                            let
                                oldsession =
                                    model.session

                                newsession =
                                    { oldsession | db = Match.new index kind (Maybe.withDefault "" model.session.user) model.session.db }
                            in
                            ( Page { model | session = newsession }, Cmd.none )

                        _ ->
                            ( Page model, Cmd.none )

                Msg.ValueChanged val ->
                    let
                        oldmodel =
                            model.page

                        newmodel =
                            { oldmodel | textfield = val }
                    in
                    ( Page { model | page = newmodel }, Cmd.none )

        _ ->
            ( Page model, Cmd.none )


updateDb : (Db.Database -> Result Update.Error Db.Database) -> Page.Page Model msg -> ( Page.Page Model msg, Cmd msg2 )
updateDb f (Page model) =
    case f model.session.db of
        Ok val ->
            let
                oldsession =
                    model.session

                newsession =
                    { oldsession | db = val }
            in
            ( Page { model | session = newsession }, Ports.toDb (Type.IO.encode Db.database.encoder val) )

        Err err ->
            let
                oldmodel =
                    model.page

                newmodel =
                    { oldmodel | error = Just (Update.errToString err) }
            in
            ( Page { model | page = newmodel }, Cmd.none )



-- VIEW


view : Page Model Msg.Msg -> Viewer.Details Msg.Msg
view (Page.Page model) =
    { detailsConfig
        | title = toTitle model.page
        , body = \_ ->
            [ h1 [] [ text "Admin Panel" ]
            , div [ class "content" ] <|
                case model.page.subpage of
                    Home ->
                        [ h3 [] [ text "This is a page that can handle sbpaths in its routing." ]
                        , div
                            []
                            (Dict.toList model.session.db.users
                                |> List.map (\( x, _ ) -> text x)
                            )

                        --                        , newEntry "answer"
                        --    , h3 [] [ text <| "The current subpath is : /" ++ String.fromInt (Maybe.withDefault -1 model.page.user_id) ]
                        -- , div [] [ text "The subpath could be anything, or a specific type, like a string or integer. You can have many levels of subpaths if you wanted!" ]
                        -- , div []
                        --     [ text " This demo accepts a single level subpath that can be any string. For example, "
                        --     , a [ href "/pagewithsubpage/xyz" ] [ text "/pagewithsubpage/xyz" ]
                        --     ]
                        -- , div [] [ a [ href "/pagewithsubpage/a-wonderful-subpath" ] [ text "click here to go to a subpath" ] ]
                        -- , div [] [ a [ href "/pagewithsubpage/i-love-elm" ] [ text "click here to go to another subpath" ] ]
                        , viewTables (Page model)
                        ]

                    Query kind id ->
                        [ p [] [ text <| "Querying " ++ Maybe.withDefault "" id ]
                        , toTable (filterKeys id kind model.session.db) kind model.session.db
                        ]

                    Edit kind id ->
                        [ div [] <| edit model.session.db kind id
                        , Maybe.map (\x -> Html.h4 [] [ text ("Error: " ++ x) ]) model.page.error
                            |> Maybe.withDefault (div [] [])
                        ]
            ]
        , search =
            case model.page.subpage of
                Query _ id ->
                    Just (Maybe.withDefault "" id)

                _ ->
                    Nothing
    }


filterKeys : Maybe String -> Db.Type -> Db.Database -> List String
filterKeys query kind db =
    case query of
        Just q ->
            Match.keys kind db
                |> List.filter (smartFilter q)

        Nothing ->
            Match.keys kind db


smartFilter : String -> String -> Bool
smartFilter first second =
    let
        diff =
            abs <| String.length first - String.length second
    in
    sift3Distance first second - toFloat diff <= 0


toTable : List String -> Db.Type -> Db.Database -> Html.Html Msg.Msg
toTable keys kind db =
    let
        headers =
            ("ID" :: Match.fields kind)
                |> List.map String.Extra.toSentenceCase

        values =
            List.map (\id -> id :: List.map (\fname -> Match.getField id fname kind db |> Maybe.withDefault "") (Match.fields kind)) keys
    in
    DataTable.dataTable 
        (DataTable.config)
        { thead =
            [ row [] <|
                List.map
                    (\x ->
                        cell
                            []
                            [ text x ]
                    )
                    headers
            ]
        , tbody =
            List.map
                (\row ->
                    DataTable.row [] <|
                        List.map
                            (\x ->
                                cell []
                                    [ text x ]
                            )
                            row
                )
                values
        }


edit : Db.Database -> Db.Type -> String -> List (Html.Html Msg.Msg)
edit db kind id =
    let
        msg =
            \x ->
                Msg.Form <|
                    Form.AttrMsg (Match.toStringPlural kind) <|
                        Form.DictMsg (Just id) <|
                            Form.AttrMsg "value" x
    in
    Match.fields kind
        |> List.map
            (\x ->
                case Match.forms id kind x db (Viewer.textForm <| Just x) of
                    Ok form ->
                        [ form
                        , Button.text
                            (Button.config |> Button.setOnClick 
                                        (Msg.Form <|
                                            Form.AttrMsg (Match.toStringPlural kind) <|
                                                Form.DictMsg (Just id) <|
                                                    Form.AttrMsg "value" <|
                                                        Form.AttrMsg x <|
                                                            Form.StringMsg (Just "Value")
                                        ))
                            "SetValue!"
                        , case
                            Db.database.toString
                                (Match.toStringPlural kind ++ "." ++ id ++ ".value." ++ x)
                                db
                          of
                            Ok v ->
                                text v

                            Err (ToString.IndexOutOfBounds index) ->
                                text ("IndexOutOfBounds: " ++ String.fromInt index)

                            Err ToString.NotFound ->
                                text "Not Found"

                            Err (ToString.NotAnInt name) ->
                                text ("Not an Int: " ++ name)

                            Err (ToString.NoSuchKey key) ->
                                text ("No such key: " ++ key)

                            Err (ToString.NoSuchSubstruct name) ->
                                text ("No such substruct: " ++ name)

                            Err ToString.NoSuchValue ->
                                text "No such value"
                        , div [] (List.map text (Match.fields kind))
                        ]

                    Err _ ->
                        [ text "ID not found"
                        , Button.text (Button.config |> Button.setOnClick (Msg.CRUD <| Msg.Create kind id []))
                            "Create!"
                        ]
            )
        |> List.map (div [])


viewTables : Page Model Msg.Msg -> Html.Html Msg.Msg
viewTables (Page model) =
    let
        db =
            model.session.db
    in
    case model.page.subpage of
        Home ->
            div []
                [ text "Database overview:"
                , ul []
                    (List.map (\x -> viewSummary x (Match.keys x db)) Match.types)
                ]

        Query kind id ->
            Html.map Msg.Admin <|
                div []
                    [ case id of
                        Just strid ->
                            viewValue strid kind db

                        Nothing ->
                            viewTable (Match.toString kind) (Match.toString kind) (Match.keys kind db)
                    ]

        Edit _ _ ->
            div []
                [ text "Edit me"
                ]



-- HELPERS


toTitle : Model -> String
toTitle _ =
    "Admin"


viewSummary : Db.Type -> List String -> Html.Html msg
viewSummary kind keys =
    let
        asString : String
        asString =
            Match.toString kind
    in
    p []
        [ text ("Table: " ++ asString ++ ": ")
        , a [ href ("/admin/" ++ Match.toString kind) ] [ text (Match.toString kind) ]
        , text ("Found " ++ (String.fromInt <| List.length <| keys) ++ " entries!")
        ]


viewTable : String -> String -> List String -> Html.Html msg
viewTable path name keys =
    div []
        [ text ("Found " ++ (String.fromInt <| List.length <| keys) ++ " " ++ name ++ "s!")
        , ul []
            (List.map (toBullet path) keys)
        ]


viewValue : String -> Db.Type -> Db.Database -> Html.Html AdminMsg
viewValue id kind db =
    let
        results =
            Match.keys kind db
                |> List.filter (String.startsWith id)
    in
    --Debug.log id <|
    if List.length results > 0 then
        viewTable (Match.toString kind) (Match.toString kind) results

    else
        div []
            [ text "No result found! Create One?"
            , Button.text (Button.config |> Button.setOnClick (Msg.AdminDb <| Msg.Create kind id []) )
                "Create!"
            ]



-- if List.member id  then
--     text "Value in Database"
-- else
--     text "Value not in Database"


toBullet : String -> String -> Html.Html msg
toBullet path name =
    li [] [ a [ href ("/admin/" ++ path ++ "/" ++ name) ] [ text name ] ]


url : Page Model Msg.Msg -> String
url (Page m) =
    let
        model =
            m.page
    in
    case model.subpage of
        Home ->
            ""

        Query kind _ ->
            Match.toString kind

        Edit kind id ->
            Match.toString kind ++ id



-- newEntry : String -> Html.Html AdminMsg
-- newEntry path =
--     path
--     |> Match.fromString
--     |> Maybe.map (\x ->Match.forms "admin" x Msg.AdminForm "*")
--     |> Maybe.map (List.map (\x -> p [] [x]))
--     |> Maybe.map (div [])
--     |> Maybe.withDefault (text "")
--"Page With Subpage - " ++ String.fromInt (Maybe.withDefault -1 model.user_id)
